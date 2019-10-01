open Bistro

module Make(P : sig val np : int val mem : int end)() : sig
  val eval : 'a workflow -> 'a
  val path : _ pworkflow -> string
  val file : _ pworkflow -> unit
  val ls : _ pworkflow -> unit
  val less : #text_file pworkflow -> unit
  val firefox : _ pworkflow -> unit
  val evince : pdf pworkflow -> unit
  val wc : #text_file pworkflow -> unit
  val rm : _ pworkflow -> unit
end
