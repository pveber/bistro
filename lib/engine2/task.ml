open Core_kernel.Std

type t = {
  id      : id ;
  descr   : string ;
  deps    : dep list ;
  cmd     : command ;
  np      : int ; (** Required number of processors *)
  mem     : int ; (** Required memory in MB *)
  timeout : int option ; (** Maximum allowed running time in hours *)
  version : int option ; (** Version number of the wrapper *)
}

and dep = [
    `Task of id
  | `Select of id * path
  | `Input of path
]
and id = string

and command =
  | Docker of Bistro.docker_image * command
  | Simple_command of token list
  | And_list of command list
  | Or_list of command list
  | Pipe_list of command list

and token =
  | S of string
  | D of dep
  | F of token list
  | DEST
  | TMP
  | NP
  | MEM

and path = string list
[@@deriving sexp]


let id t = t.id

let requirement t =
  Allocator.Request { np = t.np ; mem = t.mem }

let perform _ = assert false

let is_done _ = assert false

let clean _ = assert false
