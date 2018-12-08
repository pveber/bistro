type path =
  | FS_path of string
  | Cache_id of string
  | Cd of path * string list

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
  | List : {
      id : string ;
      elts : 'a t list ;
    } -> 'a list t
  | Eval_path : { id : string ; workflow : path t } -> string t
  | Spawn : {
      id : string ;
      elts : 'a list t ;
      f : 'a t -> 'b t ;
      deps : any list ;
    } -> 'b list t

  | Input : { id : string ; path : string ; version : int option } -> path t
  | Select : {
      id : string ;
      dir : path t ;
      sel : string list ;
    } -> path t
  | Value : ((unit -> 'a) t, any) step -> 'a t
  | Path : ((string -> unit) t, any) step -> path t
  | Shell : (shell_command, any) step -> path t

and ('a, 'b) step = {
  id : string ;
  descr : string ;
  task : 'a ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
  deps : 'b list ;
}

and shell_command = path t Command.t

and any = Any : _ t -> any

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let id : type s. s t -> string = function
  | Input { id ; _ } -> id
  | Select { id ; _ } -> id
  | Value { id ; _ } -> id
  | Path { id ; _ } -> id
  | Pure { id ; _ } -> id
  | App { id ; _ } -> id
  | Spawn { id ; _ } -> id
  | Both { id ; _ } -> id
  | Eval_path { id ; _ } -> id
  | Shell { id ; _ } -> id
  | List { id ; _ } -> id

let any x = Any x

module Any = struct
  type t = any

  let id (Any w) = id w

  let compare x y =
    String.compare (id x) (id y)

  let equal x y =
    String.equal (id x) (id y)

  let hash x = Hashtbl.hash (id x)

  let deps (Any w) = match w with
    | Pure _ -> []
    | App app -> [ Any app.f ; Any app.x ]
    | Both p -> [ Any p.fst ; Any p.snd ]
    | List l -> List.map any l.elts
    | Eval_path { workflow ; _ } -> [ Any workflow ]
    | Spawn s -> s.deps
    | Input _ -> []
    | Select sel -> [ any sel.dir ]
    | Value v -> v.deps
    | Path p -> p.deps
    | Shell s -> s.deps
end

let input ?version path =
  let id = digest (`Input, path, version) in
  Input { id ; path ; version }

let select dir sel =
  let dir, sel =
    match dir with
    | Select { dir ; sel = root ; _ } -> dir, root @ sel
    | Input _ | Path _ -> dir, sel
    | _ -> assert false
  in
  let id = digest ("select", id dir, sel) in
  Select { id ; dir ; sel }

let cached_value ?(descr = "") ?(np = 1) ?(mem = 0) ?version workflow =
  let id = digest (`Value, id workflow, version) in
  Value { id ; descr ; task = workflow ; np ; mem ; version ; deps = [ any workflow ] }

let cached_path ?(descr = "") ?(np = 1) ?(mem = 0) ?version workflow =
  let id = digest (`Value, id workflow, version) in
  Path { id ; descr ; task = workflow ; np ; mem ; version ; deps = [ any workflow ] }

let pure ~id value = Pure { id ; value }
let pure_data value = pure ~id:(digest value) value
let int = pure_data
let string = pure_data
let app f x =
  let id = digest (`App, id f, id x) in
  App { id ; f ; x }
let ( $ ) = app
let both fst snd =
  let id = digest (`Both, id fst, id snd) in
  Both { id ; fst ; snd }

let eval_path w = Eval_path { id = digest (`Eval_path, id w) ; workflow = w }

let digestible_cmd = Command.map ~f:id

let shell
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    cmds =
  let cmd = Command.And_list cmds in
  let id = digest ("shell", version, digestible_cmd cmd) in
  let deps = Command.deps cmd |> List.map any in
  Shell { descr ; task = cmd ; np ; mem ; version ; id ; deps }

let list elts =
  let id = digest ("list", List.map id elts) in
  List { id ; elts }

module Set = Set.Make(Any)
module Table = Hashtbl.Make(Any)
module Map = Map.Make(Any)

let rec independent_workflows_aux cache w ~from:u =
  if Any.equal w u then Map.add w (true, Set.empty) cache
  else if Map.mem w cache then cache
  else (
    let deps = Any.deps w in
    let f acc w = independent_workflows_aux acc w ~from:u in
    let cache = List.fold_left f cache deps in
    let children = List.map (fun k -> Map.find k cache) deps in
    if List.exists fst children
    then
      let union =
        List.fold_left
          (fun acc (_, s) -> Set.union acc s)
          Set.empty children in
      Map.add w (true, union) cache
    else Map.add w (false, Set.singleton w) cache
  )

let independent_workflows w ~from:u =
  let cache = independent_workflows_aux Map.empty w ~from:u in
  Map.find w cache |> snd |> Set.elements

let spawn elts ~f =
  let hd = pure ~id:"__should_never_be_executed__" List.hd in
  let u = app hd elts in
  let f_u = f u in
  let id = digest (`Spawn, id elts, id f_u) in
  let deps = any elts :: independent_workflows (any f_u) ~from:(any u) in
  Spawn { id ; elts ; f ; deps }

module Pretty = struct
  type 'a workflow = 'a t
  type t =
    | Pure of { id : string }
    | App of {
        id : string ;
        f : t ;
        x : t ;
      }
    | Both of {
        id : string ;
        fst : t ;
        snd : t ;
      }
    | List of {
        id : string ;
        elts : t list ;
      }
    | Eval_path of { id : string ; workflow : t }
    | Spawn of {
        id : string ;
        elts : t ;
        deps : t list ;
      }
    | Input of { id : string ; path : string ; version : int option }
    | Select of {
        id : string ;
        dir : t ;
        sel : string list ;
      }
    | Value of (t, t) step
    | Path of (t, t) step
    | Shell of (t Command.t, t) step

  let rec of_workflow : type u. u workflow -> t = function
    | Pure { id ; _ } -> Pure { id }
    | App { id ; f ; x } -> App { id ; f = of_workflow f ; x = of_workflow x }
    | Both { id ; fst ; snd } -> Both { id ; fst = of_workflow fst ; snd = of_workflow snd }
    | List { id ; elts } -> List { id ; elts = List.map of_workflow elts }
    | Eval_path { id ; workflow } -> Eval_path { id ; workflow = of_workflow workflow }
    | Spawn { id ; elts ; deps ; _ } -> Spawn { id ; elts = of_workflow elts ; deps = of_deps deps }
    | Input { id ; path ; version } -> Input { id ; path ; version }
    | Select { id ; dir ; sel } -> Select { id ; dir = of_workflow dir ; sel }
    | Value s -> Value { s with task = of_workflow s.task ;
                                deps = of_deps s.deps }
    | Path s -> Path { s with task = of_workflow s.task ;
                               deps = of_deps s.deps }
    | Shell s -> Shell { s with task = of_command s.task ;
                                deps = of_deps s.deps }

  and of_deps d = List.map (fun (Any w) -> of_workflow w) d

  and of_command cmd = Command.map cmd ~f:of_workflow
end
