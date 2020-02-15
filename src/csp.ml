(* ocaml-csp

   MIT License

   Copyright (c) 2020 Hisabumi Hatsugai

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

exception Error of string

type 'a link =
  | Nil
  | Link of
      { mutable next : 'a link
      ; mutable prev : 'a link
      ; data : 'a
      }

type 'a t =
  { mutable head : 'a link
  ; mutable tail : 'a link
  }

type process_state =
  | RUNNING
  | DORMANT
  | OMEGA

type process =
  { mutable state : process_state
  ; mutable parent : process option
  ; mutable children : process list
  ; mutable cont : unit -> unit
  ; mutable seq : (unit -> unit) list
  ; mutable seq_stack : (unit -> unit) list list
  }

type event =
  { pollFn : unit -> bool
  ; doFn : unit -> unit
  ; blockFn : bool ref -> unit
  }

type 'a chan =
  { chid : int
  ; sendQ : (bool ref * process * 'a * (unit -> unit)) t
  ; mutable recv : (bool ref * process * ('a -> bool) * ('a -> unit)) option
  }

let create () = { head = Nil; tail = Nil }

let add_tail q x =
  let p = Link { next = Nil; prev = q.tail; data = x } in
  match q.tail with
  | Nil ->
    q.head <- p;
    q.tail <- p
  | Link r ->
    r.next <- p;
    q.tail <- p

let del_link q p =
  match p with
  | Nil -> ()
  | Link r ->
    ( match r.next with
    | Nil -> ()
    | Link s -> s.prev <- r.prev );
    ( match r.prev with
    | Nil -> ()
    | Link s -> s.next <- r.next );
    if q.head == p then q.head <- r.next;
    if q.tail == p then q.tail <- r.prev

let chid ch = ch.chid

let make_process parent thunk =
  { state = RUNNING
  ; parent
  ; children = []
  ; cont = thunk
  ; seq = []
  ; seq_stack = []
  }

let dummy_process = make_process None (fun () -> ())

let cur_process = ref dummy_process

let rdyQ : process Queue.t = Queue.create ()

let next_chid = ref 0

let error msg = raise (Error msg)

let make_chan () =
  let chid = !next_chid in
  next_chid := !next_chid + 1;
  { chid; sendQ = create (); recv = None }

let dispatch () =
  if Queue.is_empty rdyQ then
    ()
  else
    let p = Queue.take rdyQ in
    cur_process := p;
    p.cont ()

let init_csp thunk =
  Queue.clear rdyQ;
  let p = make_process None thunk in
  Queue.add p rdyQ;
  dispatch ()

let not_omega p = p.state <> OMEGA

let rec propagate p_opt =
  match p_opt with
  | Some p -> (
    if p.state = DORMANT && List.exists not_omega p.children then
      dispatch ()
    else
      match p.seq with
      | [] -> (
        match p.seq_stack with
        | [] ->
          p.state <- OMEGA;
          propagate p.parent
        | s :: ss -> (
          p.seq_stack <- ss;
          match s with
          | [] -> error "propagate"
          | k :: ks ->
            p.seq <- ks;
            cur_process := p;
            k () ) )
      | k :: ks ->
        p.seq <- ks;
        cur_process := p;
        k () )
  | None -> ()

let skip () = propagate (Some !cur_process)

let par thunk_list =
  let ps =
    List.map (fun thunk -> make_process (Some !cur_process) thunk) thunk_list
  in
  !cur_process.state <- DORMANT;
  !cur_process.children <- ps;
  List.iter (fun p -> Queue.add p rdyQ) ps;
  dispatch ()

let seq thunk_list =
  match thunk_list with
  | [] -> skip ()
  | k :: ks ->
    if !cur_process.seq <> [] then
      !cur_process.seq_stack <- !cur_process.seq :: !cur_process.seq_stack;
    !cur_process.seq <- ks;
    k ()

let sendEvt ch msg k =
  let sender = !cur_process in
  let pollFn () =
    match ch.recv with
    | None -> false
    | Some (flg, _, g, _) ->
      if !flg then (
        ch.recv <- None;
        false
      ) else
        g msg
  and doFn () =
    match ch.recv with
    | None -> error "sendEvt doFn"
    | Some (flg, receiver, _, recvK) ->
      flg := true;
      sender.cont <- k;
      Queue.add sender rdyQ;
      cur_process := receiver;
      recvK msg
  and blockFn flg = add_tail ch.sendQ (flg, sender, msg, k) in
  { pollFn; doFn; blockFn }

let recvEvt ch g k =
  let receiver = !cur_process in
  let cache = ref Nil in
  let pollFn () =
    let rec pollQ p =
      match p with
      | Nil -> false
      | Link r ->
        let flg, _, msg, _ = r.data in
        if !flg then (
          let p' = r.next in
          del_link ch.sendQ p;
          pollQ p'
        ) else if g msg then (
          cache := p;
          true
        ) else
          pollQ r.next
    in
    pollQ ch.sendQ.head
  and doFn () =
    match !cache with
    | Nil -> error "recvEvt doFn"
    | Link r ->
      let flg, sender, msg, senderK = r.data in
      flg := true;
      sender.cont <- senderK;
      Queue.add sender rdyQ;
      cur_process := receiver;
      k msg
  and blockFn flg = ch.recv <- Some (flg, receiver, g, k) in
  { pollFn; doFn; blockFn }

let sync event =
  if event.pollFn () then
    event.doFn ()
  else
    let dirtyFlg = ref false in
    event.blockFn dirtyFlg;
    dispatch ()

let send ch msg k = sync (sendEvt ch msg k)

let recv ch g k = sync (recvEvt ch g k)

let select event_list =
  let poll e = e.pollFn () in
  match List.find_opt poll event_list with
  | Some e -> e.doFn ()
  | None ->
    let dirtyFlg = ref false in
    let block e = e.blockFn dirtyFlg in
    List.iter block event_list;
    dispatch ()

let inject ch msg =
  match ch.recv with
  | None -> error "inject"
  | Some (flg, receiver, g, k) ->
    if !flg || not (g msg) then
      error "inject"
    else (
      ch.recv <- None;
      flg := true;
      cur_process := receiver;
      k msg
    )
