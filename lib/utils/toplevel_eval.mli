open Bistro

module Make(P : sig val np : int val mem : int end)() : sig
  val eval : 'a workflow -> 'a
  val path : _ path workflow -> string
  val file : _ file -> unit
  val ls : _ path workflow -> unit
  val less : #text file -> unit
  val firefox : _ path workflow -> unit
  val evince : pdf file -> unit
  val wc : #text file -> unit
  val rm : _ path workflow -> unit
end
