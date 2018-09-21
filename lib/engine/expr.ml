open Base
open Bistro

module I = struct
  type _ t =
    | Pure : 'a -> 'a t
    | App   : ('a -> 'b) t * 'a t -> 'b t
    | Pair   : 'a t * 'b t -> ('a * 'b) t
    | List : 'a t list -> 'a list t
    | Workflow_path : _ workflow t -> string t
    | Spawn : 'a list t * ('a t -> 'b t) -> 'b list t
    | Glob : {
        dir : _ #directory workflow t ;
        pattern : string option ;
      } -> (string * _ workflow) list t

  let pure x = Pure x
  let app f x = App (f, x)
  let ( $ ) = app
  let map x ~f = pure f $ x
  let both x y = Pair (x, y)
  let pair = both
  let dep x = Workflow_path x
  let deps xs = Spawn (xs, fun x -> Workflow_path x)
  let list xs = List xs
  let return x = pure x
  let glob_full ?pattern dir = Glob { dir ; pattern }

  let list_map xs ~f = map xs ~f:(List.map ~f)
  let spawn xs ~f = Spawn (xs, f)

  let glob ?pattern dir =
    glob_full ?pattern dir
    |> list_map ~f:snd

end


module Let_syntax = struct
  module Let_syntax = struct
    include I
    module Open_on_rhs = struct
      include I
    end
  end
end

include I

(* let bowtie w = *)
(*   let%map size = file_size w in *)
(*   let mem = `GB size / 10 in *)
(*   bowtie ~mem w *)
