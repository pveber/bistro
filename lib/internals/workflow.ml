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
  | Eval_path : { id : string ; workflow : path t } -> string t
  | Spawn : {
      id : string ;
      elts : 'a list t ;
      f : 'a t -> 'b t ;
    } -> 'b list t

  | Input : { id : string ; path : string ; version : int option } -> path t
  | Select : {
      id : string ;
      dir : path t ;
      sel : string list ;
    } -> path t
  | Value : (unit -> 'a) t step -> 'a t
  | Path : (string -> unit) t step -> path t
  | Shell : shell_command step -> path t

and 'a step = {
  id : string ;
  descr : string ;
  task : 'a ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}

and shell_command = path t Command.t

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
  Value { id ; descr ; task = workflow ; np ; mem ; version }

let cached_path ?(descr = "") ?(np = 1) ?(mem = 0) ?version workflow =
  let id = digest (`Value, workflow, version) in
  Path { id ; descr ; task = workflow ; np ; mem ; version }

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

let spawn elts ~f =
  let hd = pure ~id:"__should_never_be_executed__" List.hd in
  let id = digest (`Spawn, id elts, id (f (app hd elts))) in
  Spawn { id ; elts ; f }

let digestible_cmd = Command.map ~f:id

let shell
    ?(descr = "")
    ?(mem = 100)
    ?(np = 1)
    ?version
    cmds =
  let cmd = Command.And_list cmds in
  let id = digest ("shell", version, digestible_cmd cmd) in
  Shell { descr ; task = cmd ; np ; mem ; version ; id }
