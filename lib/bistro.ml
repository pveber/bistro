type 'a path = string

class type directory = object
  method file_kind : [`directory]
end

module Workflow = struct
  type _ t =
    | Pure : { id : string ; value : 'a } -> 'a t
    | App : ('a -> 'b) t * 'a t -> 'b t
    | Both : 'a t * 'b t -> ('a *'b) t
    | Eval_path : string t -> string t
    | Spawn : {
        elts : 'a list t ;
        f : 'a t -> 'b t ;
      } -> 'b list t

    | Input : { id : string ; path : string } -> string t
    | Select : {
        id : string ;
        dir : #directory path t ;
        sel : string list ;
      } -> string t
    | Value : (unit -> 'a) step -> 'a t
    | Path : (string -> unit) step -> string t

  and 'a step = {
    id : string ;
    descr : string ;
    workflow : 'a t ;
  }

  let reveal x = x
end

type 'a workflow = 'a Workflow.t

open Workflow

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
let pure_data value = pure ~id:(digest value) value
let app f x = App (f, x)
let both x y = Both (x, y)
let eval_path w = Eval_path w
let spawn elts ~f = Spawn { elts ; f }

module Internals = struct
  module Workflow = Workflow
end
