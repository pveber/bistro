open Bistro_base

type _ t =
  | Return : 'a -> 'a t
  | Bind   : 'a t * ('a -> 'b t) -> 'b t
  | Pair   : 'a t * 'b t -> ('a * 'b) t
  | List : 'a t list -> 'a list t
  | Workflow_path : _ Workflow.t -> string t
  | Spawn : 'a list t * ('a -> 'b t) -> 'b list t
  | Glob : {
      dir : _ #File_formats.directory Workflow.t ;
      pattern : string option ;
    } -> (string * _ Workflow.t) list t

module Let_syntax = struct
  module Let_syntax = struct
    let return x = Return x
    let bind x ~f = Bind (x, f)
    let map x ~f = Bind (x, fun x -> return (f x))
    let both x y = Pair (x, y)
    module Open_on_rhs = struct
      let dep x = Workflow_path x
      let deps xs = Spawn (xs, fun x -> Workflow_path x)
      let list xs = List xs
      let return x = Return x
      let map x ~f = Bind (x, fun x -> return (f x))
      let list_map_bind x ~f = Spawn (x, f)
      let list_map  x ~f = Spawn (x, fun x -> return (f x))
      let pair x y = Pair (x, y)
      let glob ?pattern dir = Glob { dir ; pattern }
    end
  end
end

include Let_syntax.Let_syntax
include Let_syntax.Let_syntax.Open_on_rhs
let spawn x ~f = Spawn (x, f)

(* let bowtie w = *)
(*   let%map size = file_size w in *)
(*   let mem = `GB size / 10 in *)
(*   bowtie ~mem w *)
