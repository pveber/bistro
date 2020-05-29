type 'a t =
  | Simple_command of 'a Template.t
  | And_list of 'a t list
  | Or_list of 'a t list
  | Pipe_list of 'a t list

val map :
  'a t ->
  f:('a -> 'b) ->
  'b t

val deps : 'a t -> 'a list
