type 'a path = string

class type directory = object
  method file_kind : [`directory]
end

type _ workflow =
  | Pure : { id : string ; value : 'a } -> 'a workflow
  | App : ('a -> 'b) workflow * 'a workflow -> 'b workflow
  | Both : 'a workflow * 'b workflow -> ('a *'b) workflow
  | Eval_path : 'a path workflow -> string workflow
  | Spawn : {
      elts : 'a list workflow ;
      f : 'a workflow -> 'b workflow ;
    } -> 'b list workflow

  | Input : { id : string ; path : string } -> 'a path workflow
  | Select : {
      id : string ;
      dir : #directory path workflow ;
      sel : string list ;
    } -> 'a path workflow
  | Value : (unit -> 'a) step -> 'a workflow
  | Path : (string -> unit) step -> 'a path workflow

and 'a step = {
  id : string ;
  descr : string ;
  workflow : 'a workflow ;
}

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let id = function
  | Input { id ; _ }
  | Select { id ; _ }
  | Value { id ; _ }
  | Path { id ; _ } -> id
  | _ -> assert false

let input path =
  let id = digest (`Input path) in
  Input { id ; path }

let select dir sel =
  let dir, sel =
    match dir with
    | Select { dir ; sel = root ; _ } -> dir, root @ sel
    | Input _ | Value _ | Path _ -> dir, sel
    | _ -> assert false
  in
  let id = digest ("select", id dir, sel) in
  Select { id ; dir ; sel }

let cached_value ?(descr = "") workflow =
  let id = digest (`Value, workflow) in
  Value { id ; descr ; workflow }

let cached_path ?(descr = "") workflow =
  let id = digest (`Value, workflow) in
  Path { id ; descr ; workflow }

let pure ~id value = Pure { id ; value }
let app f x = App (f, x)
let both x y = Both (x, y)
let eval_path w = Eval_path w
let spawn elts ~f = Spawn { elts ; f }
