open Bistro_base

(* type 'a code = { id : string ; value : 'a } *)

(* type _ t = *)
(*   | Return : 'a code -> 'a t *)
(*   | Bind   : 'a t * ('a -> 'b t) code -> 'b t *)
(*   | Pair   : 'a t * 'b t -> ('a * 'b) t *)
(*   | List : 'a t list -> 'a list t *)
(*   | Workflow_path : _ Workflow.t -> string t *)
(*   | Spawn : { *)
(*       xs : 'a list t ; *)
(*       f  : ('a -> 'b t) code ; *)
(*     } -> 'b list t *)

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
    } -> _ Workflow.t list t

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

include Let_syntax.Let_syntax.Open_on_rhs

(* let bowtie w = *)
(*   let%map size = file_size w in *)
(*   let mem = `GB size / 10 in *)
(*   bowtie ~mem w *)

(* type _ t = *)
(*   | PureW : 'a Workflow.t -> 'a Workflow.t t *)
(*   | PureF : ('a Workflow.t -> 'b Workflow.t) -> ('a Workflow.t -> 'b Workflow.t) t *)
(*   | App : ('a -> 'b) t * 'a t -> 'b t *)
(*   | Spawn : { *)
(*       xs : 'a list t ; *)
(*       f  : 'a t -> 'b t ; *)
(*     } -> 'b list t *)

(* let pure ~id value = Pure { id ; value } *)
(* let param value = Pure { id = Workflow.digest value ; value } *)
(* let pureW w = Pure { id = Workflow.id w ; value = w } *)
(* let app f x = App (f, x) *)
(* let ( $ ) f x = app f x *)

(* let fst = Pure { id = "Pervasives.fst" ; value = fst } *)
(* let snd = Pure { id = "Pervasives.snd" ; value = snd } *)
(* let pair x y = Pair (x, y) *)


(* let collection_map xs f = *)
(*   list_map xs ~f:(fun tup -> *)
(*       pair (fst $ tup) (f (snd $ tup)) *)
(*     ) *)

(* let a = list_map (list []) ~f:(fun w -> Workflow_path w) *)

(* let glob dir = *)
(*   Pure { id = "glob" ; *)
(*          value = fun path -> Sys.readdir path |> Array.to_list |> List.map (fun fn -> Workflow.select dir [fn]) } *)
(*   $ Workflow_path (pureW dir) *)
