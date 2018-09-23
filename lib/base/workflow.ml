open Base

let digest x =
  Caml.(Digest.to_hex (Digest.string (Marshal.to_string x [])))

type t =
  | Input of { id : string ; path : string }
  | Select of {
      id : string ;
      dir : t ; (* invariant: [dir] is not a select *)
      sel : string list
    }
  | Shell of shell
  | Plugin of (env -> unit) step

and 'a step = {
  id : string ;
  descr : string ;
  task : 'a ;
  deps : dep list ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}
and shell = shell_command step
and shell_command = dep Command.t
and template = dep Template.t
and env = <
  tmp : string ;
  dest : string ;
  np : int ;
  mem : int
>
and dep = WDep of t | WLDep of t_list
and t_list =
  | List of {
      id : string ;
      elts : t list ;
    }
  | Glob of {
      id : string ;
      dir : t ;
      pattern : string option ;
    }
  | ListMap of {
      id : string ;
      elts : t_list ;
      f : t -> t ;
    }

let id = function
  | Input { id ;  _ }
  | Select { id ;  _}
  | Shell { id ; _ } -> id
  | Plugin { id ; _  } -> id

let compare u v =
  String.compare (id u) (id v)

let equal x y =
  compare x y = 0

let select u q =
  let k dir sel =
    let id = digest ("select", id u, sel) in
    Select { id ; dir ; sel }
  in
  match u with
  | Select { dir ; sel = p ; _ } -> k dir (p @ q)
  | Input _
  | Plugin _
  | Shell _ -> k u q

let input ?version path =
  let id = digest ("input", path, version) in
  Input { id ; path }

let rec digestible_workflow = function
  | Shell s -> `Shell s.id
  | Input { path ; _ } -> `Input path
  | Select { dir = Input { path = p ; _ } ; sel = q ; _ } ->
    `Select ((`Input p), q)
  | Select { dir ; sel = p ; _ } ->
    `Select (digestible_workflow dir, p)
  | Plugin c -> `Plugin c.id

let digestible_workflow_list = function
  | List l -> `List l.id
  | Glob g -> `Glob g.id
  | ListMap lm -> `ListMap lm.id

let digestible_dep = function
  | WDep w -> digestible_workflow w
  | WLDep wl -> digestible_workflow_list wl

let digestible_cmd = Command.map ~f:digestible_dep

let shell
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    cmds =
  let cmd = Command.And_list cmds in
  let id = digest ("shell", version, digestible_cmd cmd) in
  let deps = Command.deps cmd in
  Shell { descr ; task = cmd ; deps ; np ; mem ; version ; id }

let plugin
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    id deps f =
  let id = digest ("closure", version, id) in
  Plugin { descr ; task = f ; deps ; np ; mem ; version ; id }

let deps = function
  | Input _ -> []
  | Select { dir ;  _ } -> [ WDep dir ]
  | Shell s -> s.deps
  | Plugin s -> s.deps
