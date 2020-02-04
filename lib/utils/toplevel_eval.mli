open Bistro

module Make(P : sig val np : int val mem : int end)() : sig
  val eval : 'a workflow -> 'a
  val path : _ path workflow -> string
  val file : _ path workflow -> unit
  val ls : _ path workflow -> unit
  val less : #text_file path workflow -> unit
  val firefox : _ path workflow -> unit
  val evince : pdf path workflow -> unit
  val wc : #text_file path workflow -> unit
  val rm : _ path workflow -> unit
end
