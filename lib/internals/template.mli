type 'a token =
  | S of string
  | D of 'a
  | F of 'a token list
  | DEST
  | TMP
  | NP
  | MEM

type 'a t = 'a token list

val map :
  'a t ->
  f:('a -> 'b) ->
  'b t

val deps : 'a t -> compare:('a -> 'a -> int) -> 'a list
