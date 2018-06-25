open Core_kernel
open File_formats

let digest x =
  Md5.to_hex (Md5.digest_string (Marshal.to_string x []))

type _ t =
  | Input : { id : string ; path : string } -> 'a t
  | Select : {
      id : string ;
      dir : _ #directory t ; (* invariant: [dir] is not a select *)
      path : string list
    } -> 'a t
  | Shell : shell -> 'a t
  | Closure : 'a closure -> 'a value t

and 'a _list_ =
  | Const_list : 'a t list -> 'a _list_
  | Glob : {
      dir : _ #directory t ;
      pattern : string ;
    } -> 'a _list_
  | List : 'a list value t -> 'a _list_
  | Map_list : {
      xs : 'a _list_ ;
      f : ('a t -> 'b t) ;
    } -> 'b _list_

and any = Any : _ t -> any

and 'a step = {
  id : string ;
  descr : string ;
  deps : any list ;
  task : 'a ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}

and shell = shell_command step

and shell_command = any Command.t
and template = any Template.t

and 'a closure = (env -> 'a) step

and env = <
  eval : 'a. 'a t -> 'a ;
  np : int ;
  mem : int ;
  tmp : string ;
  dest : string
>

let id : type s. s t -> string = function
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
  | Input _  -> k u q
  | Shell _ -> k u q

let deps : type s. s t -> any list = function
  | Input _ -> []
  | Select { dir ; _ } -> [ Any dir ]
  | Closure { deps ; _ } -> deps
  | Shell { deps ; _ } -> deps
  (* | Map_list { xs ; _ } -> (
   *     match xs with
   *     | Const_list xs -> List.map xs ~f:(fun (_, w) -> Dep w)
   *     | Glob { dir ; _ } -> [ Dep dir ]
   *     | List xs -> [ Dep xs ]
   *   ) *)

let input ?(may_change = false) path =
  let hash = if may_change then Some (Md5.digest_file_blocking_without_releasing_runtime_lock path) else None in
  let id = digest ("input", path, hash) in
  Input { id ; path }


let digestible_dep : type s. s t -> _ = function
  | Shell s -> `Shell s.id
  | Input { path ; _ } -> `Input path
  | Select { dir = Input { path = p ; _ } ; path = q } ->
    `Select ((`Input p), q)
  | Select { dir = Shell s ; path = p ; _ } ->
    `Select (`Shell s.id, p)
  | Select { dir = Select _ ; _ } -> assert false
  | Closure c -> `Closure c.id

let rec digestible_dep_list : type s. s _list_ -> _ = function
  | Const_list xs -> `Const_list (List.map xs ~f:id)
  | List value -> `List (id value)
  | Map_list { xs ; f } ->
    `Map_list (digestible_dep_list xs, digestible_dep (f (input "foobar")))
  | Glob { dir ; pattern } -> `Glob (digestible_dep dir, pattern)

let digestible_cmd = Command.map ~f:(function
    | Any u -> `Dep (id u)
    (* | Deps { xs ; _ } -> digestible_dep_list xs *)
  )

let digestible_deps xs =
  List.map xs ~f:(function
      | Any d -> digestible_dep d
      (* | Deps { xs } -> digestible_dep_list xs *)
    )

let shell
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    cmds =
  let cmd : any Command.t = Command.And_list cmds in
  let deps = Command.deps cmd in
  let id = digest ("shell", version, digestible_cmd cmd, digestible_deps deps) in
  Shell { descr ; deps ; task = cmd ; np ; mem ; version ; id }
