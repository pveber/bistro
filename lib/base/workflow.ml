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
  | Plugin of plugin
  | MapDir of {
      id : string ;
      pattern : string option ;
      dir : t ;
      f : t -> t ;
      deps : t list ;
    }

and 'a step = {
  id : string ;
  descr : string ;
  task : 'a ;
  deps : t list ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}
and shell = shell_command step
and plugin = (env -> unit) step
and shell_command = t Command.t
and template = t Template.t
and env = <
  tmp : string ;
  dest : string ;
  np : int ;
  mem : int
>

let id = function
  | Input { id ;  _ }
  | Select { id ;  _}
  | Shell { id ; _ }
  | Plugin { id ; _  }
  | MapDir { id ; _ } -> id

let deps = function
  | Input _ -> []
  | Select { dir ;  _ } -> [ dir ]
  | MapDir s -> s.deps
  | Shell s -> s.deps
  | Plugin s -> s.deps

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
  | Shell _
  | MapDir _ -> k u q

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
  | MapDir md -> `MapDir md.id

let digestible_cmd = Command.map ~f:digestible_workflow

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

module S = Caml.Set.Make(struct type nonrec t = t let compare = compare end)
module M = Caml.Map.Make(struct type nonrec t = t let compare = compare end)

let rec independant_workflows_aux cache w ~from:u =
  if equal w u then M.add w (true, S.empty) cache
  else if M.mem w cache then cache
  else (
    let cache = List.fold (deps w) ~init:cache ~f:(fun acc w ->
        independant_workflows_aux acc w ~from:u
      )
    in
    let children = List.map (deps w) ~f:(Fn.flip M.find cache) in
    if List.exists children ~f:fst
    then
      let union = List.fold children ~init:S.empty ~f:(fun acc (_, s) -> S.union acc s) in
      M.add w (true, union) cache
        else M.add w (false, S.singleton w) cache
  )

let independant_workflows w ~from:u =
  let cache = independant_workflows_aux M.empty w ~from:u in
  M.find w cache |> snd |> S.elements

let mapdir ?pattern dir ~f =
  let u = input "string that could'nt possibly be a filename" in
  let f_u = f u in
  let id = digest ("mapdir", id dir, id f_u) in
  let deps = independant_workflows f_u ~from:u in
  MapDir { id ; pattern ; dir ; f ; deps }
