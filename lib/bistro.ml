type 'a path = string

class type directory = object
  method file_kind : [`directory]
end

type _ workflow =
  | Input : { id : string ; path : string } -> 'a path workflow
  | Select : {
      id : string ;
      dir : #directory path workflow ;
      sel : string list ;
    } -> 'a path workflow
  | Value : 'a step -> 'a workflow
  | Path : (string -> unit) step -> 'a path workflow

and _ expr =
  | Pure : { id : string ; value : 'a } -> 'a expr
  | App : ('a -> 'b) expr * 'a expr -> 'b expr
  | Both : 'a expr * 'b expr -> ('a *'b) expr
  | Eval_workflow : 'a workflow -> 'a expr
  | Spawn : {
      elts : 'a list expr ;
      f : 'a expr -> 'b expr ;
    } -> 'b list expr

and 'a step = {
  id : string ;
  descr : string ;
  expr : 'a expr ;
}

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

module Workflow = struct
  type 'a t = 'a workflow

  let id = function
    | Input { id ; _ }
    | Select { id ; _ }
    | Value { id ; _ }
    | Path { id ; _ } -> id

  let input path =
    let id = digest (`Input path) in
    Input { id ; path }

  let select dir sel =
    let dir, sel =
      match dir with
      | Select { dir ; sel = root ; _ } -> dir, root @ sel
      | Input _ | Value _ | Path _ -> dir, sel
    in
    let id = digest ("select", id dir, sel) in
    Select { id ; dir ; sel }

  let make ?(descr = "") expr =
    let id = digest (`Value, expr) in
    Value { id ; descr ; expr }

  let makep ?(descr = "") expr =
    let id = digest (`Value, expr) in
    Path { id ; descr ; expr }
end

module Expr = struct
  type 'a t = 'a expr
  let pure ~id value = Pure { id ; value }
  let app f x = App (f, x)
  let both x y = Both (x, y)
  let eval_workflow w = Eval_workflow w
  let eval_path w = Eval_workflow w
  let spawn elts ~f = Spawn { elts ; f }
end
