type 'a path = string

class type directory = object
  method file_kind : [`directory]
end

module Workflow = struct
  type _ t =
    | Pure : { id : string ; value : 'a } -> 'a t
    | App : {
        id : string ;
        f : ('a -> 'b) t ;
        x : 'a t ;
      } -> 'b t
    | Both : {
        id : string ;
        fst : 'a t ;
        snd : 'b t ;
      } -> ('a *'b) t
    | Eval_path : { id : string ; workflow : string t } -> string t
    | Spawn : {
        id : string ;
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

let id : type s. s workflow -> string = function
  | Input { id ; _ } -> id
  | Select { id ; _ } -> id
  | Value { id ; _ } -> id
  | Path { id ; _ } -> id
  | Pure { id ; _ } -> id
  | App { id ; _ } -> id
  | Spawn { id ; _ } -> id
  | Both { id ; _ } -> id
  | Eval_path { id ; _ } -> id

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
  let id = digest (`Value, id workflow) in
  Value { id ; descr ; workflow }

let cached_path ?(descr = "") workflow =
  let id = digest (`Value, workflow) in
  Path { id ; descr ; workflow }

let pure ~id value = Pure { id ; value }
let pure_data value = pure ~id:(digest value) value
let int = pure_data
let string = pure_data
let app f x =
  let id = digest (`App, id f, id x) in
  App { id ; f ; x }
let both fst snd =
  let id = digest (`Both, id fst, id snd) in
  Both { id ; fst ; snd }

let eval_path w = Eval_path { id = digest (`Eval_path, id w) ; workflow = w }



let spawn elts ~f =
  let hd = pure ~id:"__should_never_be_executed__" List.hd in
  let id = digest (`Spawn, id elts, id (f (app hd elts))) in
  Spawn { id ; elts ; f }

module Internals = struct
  module Workflow = Workflow
end
