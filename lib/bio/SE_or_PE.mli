type 'a t =
  | Single_end of 'a
  | Paired_end of 'a * 'a

val map : 'a t -> f:('a -> 'b) -> 'b t
val fst : 'a t -> 'a
