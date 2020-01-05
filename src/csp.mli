(*

ocaml-csp

MIT License

Copyright (c) 2020 Hisabumi Hatsugai

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*)

type event
type 'a chan

val make_chan : unit -> 'a chan
val chid : 'a chan -> int
val init_csp : (unit -> unit) -> unit
val skip : unit -> unit
val par : (unit -> unit) list -> unit
val seq : (unit -> unit) list -> unit
val sendEvt : 'a chan -> 'a -> (unit -> unit) -> event
val recvEvt : 'a chan -> ('a -> bool) -> ('a -> unit) -> event
val sync : event -> unit
val send : 'a chan -> 'a -> (unit -> unit) -> unit
val recv : 'a chan -> ('a -> bool) -> ('a -> unit) -> unit
val select : event list -> unit
val inject : 'a chan -> 'a -> unit
