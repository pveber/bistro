open Core_kernel
open File_formats

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

type u =
  | Input of { id : string ; path : string }
  | Select of {
      id : string ;
      dir : u ; (* invariant: [dir] is not a select *)
      path : string list
    }
  | Shell of shell
  | Closure of (env -> unit) expr step

and 'a t = u

and 'a expr =
  | Pure : {
      id : string ;
      value : 'a ;
    } -> 'a expr
  | App : ('a ->'b) expr * 'a expr -> 'b expr
  | List : 'a expr list -> 'a list expr
  | Glob : {
      dir : _ #directory t ;
      pattern : string ;
    } -> 'a t list expr
  | Map_workflows : {
      xs : 'a t list expr ;
      f : ('a t -> 'b t) ;
    } -> 'b t list expr
  | Dep   : _ t expr -> string expr
  | Deps : _ t list expr -> string list expr

and 'a step = {
  id : string ;
  descr : string ;
  task : 'a ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}
and shell = shell_command step
and shell_command = string expr Command.t
and template = string expr Template.t
and env = < tmp : string ; dest : string  ; np : int ; mem : int >

let id = function
  | Input { id ;  _ }
  | Select { id ;  _}
  | Shell { id ; _ } -> id
  | Closure { id ; _  } -> id

let compare u v =
  String.compare (id u) (id v)

let equal x y =
  compare x y = 0

let select u q =
  let k dir path =
    let id = digest ("select", id u, path) in
    Select { id ; dir ; path }
  in
  match u with
  | Select { dir ; path = p ; _ } -> k dir (p @ q)
  | Input _
  | Closure _
  | Shell _ -> k u q

let input ?(may_change = false) path =
  let hash = if may_change then Some (Md5.digest_file_blocking_without_releasing_runtime_lock path) else None in
  let id = digest ("input", path, hash) in
  Input { id ; path }


let rec digestible_dep = function
  | Shell s -> `Shell s.id
  | Input { path ; _ } -> `Input path
  | Select { dir = Input { path = p ; _ } ; path = q } ->
    `Select ((`Input p), q)
  | Select { dir ; path = p ; _ } ->
    `Select (digestible_dep dir, p)
  | Closure c -> `Closure c.id


let rec digestible_expr : type s. s expr -> _ = function
  | Pure { id } -> `Pure id
  | App (f, x) -> `App (digestible_expr f, digestible_expr x)
  | List xs -> `List (List.map xs ~f:digestible_expr)
  | Glob { dir ; pattern } -> `Glob (digestible_dep dir, pattern)
  | Map_workflows { xs ; f } ->
    `Map_workflows (digestible_expr xs, digestible_dep (f (input "foobar")))
  | Dep w -> `Dep (digestible_expr w)
  | Deps ws -> `Deps (digestible_expr ws)

let digestible_cmd = Command.map ~f:digestible_expr

let shell
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    cmds =
  let cmd = Command.And_list cmds in
  let id = digest ("shell", version, digestible_cmd cmd) in
  Shell { descr ; task = cmd ; np ; mem ; version ; id }

let pureW w = Pure { id = id w ; value = w }
let dep e = Dep e

