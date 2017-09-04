open Core_kernel.Std

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

module Path = struct
  type t = string list
  [@@deriving sexp]

  let rec normalize = function
    | "." :: t
    | "" :: t -> normalize t
    | h :: ".." :: t ->
      let t = normalize t in
      if h <> ".." then t
      else ".." :: ".." :: t
    | h :: t -> h :: normalize t
    | [] -> []

  let common_prefix p q =
    let rec aux res p q =
      match p, q with
      | h_p :: t_p, h_q :: t_q when h_p = h_q ->
        aux (h_p :: res) t_p t_q
      | _ -> List.rev res, p, q
    in
    aux [] p q

  let of_string x =
    match String.split ~on:'/' x with
    | "" :: t -> "/" :: t
    | xs -> xs

  let make_relative ?from:(q = Sys.getcwd ()) p =
    if Filename.is_relative q
    then invalid_argf "make_rel_path: base %s should be absolute" q ()
    else if Filename.is_relative p then of_string p
    else
      let p = normalize (of_string p)
      and q = normalize (of_string q) in
      let _, p_suffix, q_suffix = common_prefix p q in
      List.map q_suffix ~f:(const "..") @ p_suffix

  let rec to_string = function
    | [] -> "."
    | "" :: t -> Filename.concat "." (to_string t)
    | "/" :: t -> "/" ^ (to_string t)
    | p -> List.reduce_exn p ~f:Filename.concat

end


type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type id = string

type dep = [
    `Task of id
  | `Select of id * Path.t
  | `Input of Path.t
]

type env = <
  dep : dep -> string ;
  np : int ;
  mem : int ;
  tmp : string ;
  dest : string
>


type 'a directory = [`directory of 'a]

class type ['a,'b] file = object
  method format : 'a
  method encoding : [< `text | `binary] as 'b
end

class type ['a] value = object
  inherit [ [`value of 'a], [`binary] ] file
end

module Command = struct
  type 'a t =
    | Docker of docker_image * 'a t
    | Simple_command of 'a token list
    | And_list of 'a t list
    | Or_list of 'a t list
    | Pipe_list of 'a t list

  and 'a token =
    | S of string
    | D of 'a
    | F of 'a token list
    | DEST
    | TMP
    | NP
    | MEM
    | EXE

  let rec deps_of_template tmpl =
    List.map tmpl ~f:(function
        | D r -> [ r ]
        | F toks -> deps_of_template toks
        | S _ | DEST | TMP | NP | MEM | EXE -> []
      )
    |> List.concat
    |> List.dedup

  let rec deps = function
    | And_list xs
    | Or_list xs
    | Pipe_list xs ->
      List.map xs ~f:deps
      |> List.concat
      |> List.dedup
    | Simple_command tokens -> deps_of_template tokens
    | Docker (_, c) -> deps c

  let rec map ~f = function
    | Docker (im, cmd) -> Docker (im, map ~f cmd)
    | Simple_command toks ->
      Simple_command (List.map ~f:(map_token ~f) toks)
    | And_list cmds -> And_list (List.map cmds ~f:(map ~f))
    | Or_list cmds -> Or_list (List.map cmds ~f:(map ~f))
    | Pipe_list cmds -> Pipe_list (List.map cmds ~f:(map ~f))

  and map_token ~f x = match x with
    | S s -> S s
    | D dep -> D (f dep)
    | F toks -> F (List.map toks ~f:(map_token ~f))
    | DEST -> DEST
    | TMP -> TMP
    | NP -> NP
    | MEM -> MEM
    | EXE -> EXE


end

type u =
  | Input of string * Path.t
  | Select of string * u * Path.t (* invariant: [u] is not a select *)
  | Step of step

and step = {
  id : string ;
  descr : string ;
  deps : u list ;
  action : action ;
  np : int ; (** Required number of processors *)
  mem : int ; (** Required memory in MB *)
  version : int option ; (** Version number of the wrapper *)
}

and action =
  | Exec of dep Command.t
  | Eval of {
      id : string ;
      f : env -> unit ;
    }

module U_impl = struct
  
  let id = function
    | Input (id, _)
    | Select (id, _, _)
    | Step { id } -> id

  let compare u v =
    String.compare (id u) (id v)

  let equal x y =
    compare x y = 0

end

module U = struct
  type t = u
  include U_impl
end

type ('a, 'b) selector = Selector of Path.t



module Workflow = struct
  type 'a t = U.t

  include U_impl

  let input ?(may_change = false) target =
    let hash = if may_change then Some (Digest.file target) else None in
    let id = digest ("input", target, hash) in
    Input (id, Path.make_relative target)

  let digestible_action = function
    | Exec cmd -> `Exec cmd
    | Eval { id } -> `Eval id

  let make
      ?(descr = "")
      ?(mem = 100)
      ?(np = 1)
      ?version
      action
      deps =
    let id = digest ("step", version, digestible_action action) in
    Step { descr ; deps ; action ; np ; mem ; version ; id }

  let of_fun ?descr ?mem ?np ?version ~id ~deps f =
    make ?descr ?mem ?np ?version (Eval { id ; f }) deps

  let select u (Selector path) =
    let u, path =
      match u with
      | Select (_, v, p) -> v, p @ path
      | Input _ | Step _ -> u, path
    in
    let id = digest ("select", id u, path) in
    Select (id, u, path)

  let rec collect accu u =
    let accu' = List.Assoc.add ~equal:String.equal accu (id u) u in
    match u with
    | Input _ -> accu'
    | Select (_, v, _) -> collect accu' v
    | Step { deps } ->
      List.fold deps ~init:accu' ~f:collect

  let descr = function
    | Input (_,p) -> (Path.to_string p)
    | Select (_, _, p) -> (Path.to_string p)
    | Step { descr } -> descr


  let u x = x

  let to_dep = function
    | Step s -> `Task s.id
    | Input (_, p) -> `Input p
    | Select (_, Input (_, p), q) ->
      `Input (p @ q)
    | Select (_, Step s, p) ->
      `Select (s.id, p)
    | Select (_, Select _, _) -> assert false

  let to_dot u oc =
    let nodes = collect [] u in
    fprintf oc "digraph workflow {\n" ;
    List.iter nodes ~f:(fun (id_n, n) ->
        match n with
        | Step { deps } ->
          fprintf oc "n%s [shape=box,label = \"%s\"];\n" id_n (descr n) ;
          List.iter deps ~f:(fun m ->
              fprintf oc "n%s -> n%s;\n" id_n (id m)
            )
        | Select (_,m,_) ->
          fprintf oc "n%s [shape=box,label = \"%s\",shape=plaintext];\n" id_n (descr n) ;
          fprintf oc "n%s -> n%s [style=dotted];\n" id_n (id m)
        | Input _ ->
          fprintf oc "n%s [label = \"%s\"];\n" id_n (descr n)
      ) ;
    fprintf oc "}\n"
end

type 'a workflow = 'a Workflow.t
type any_workflow = Any_workflow : _ Workflow.t -> any_workflow

module Template = struct
  type t = U.t Command.token list

  let dest = [ Command.DEST ]
  let tmp = [ Command.TMP ]
  let np = [ Command.NP ]
  let mem = [ Command.MEM ]
  let exe = [ Command.EXE ]

  let string s = [ Command.S s ]
  let int i = string (string_of_int i)
  let float f = string (Float.to_string f)
  let path p = string (Path.to_string p)
  let dep w = [ Command.D w ]

  let f ?a:(c = 1) () = c
  let quote ?using:(c = '"') e =
    let quote_symbol = Command.S (Char.to_string c) in
    quote_symbol :: e @ [ quote_symbol ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:(string sep)
    |> List.concat

  let seq ?sep xs =
    let format = match sep with
      | None -> ident
      | Some sep -> List.intersperse ~sep:(string sep)
    in
    List.concat (format xs)

  let enum dic x = string (List.Assoc.find_exn ~equal:( = ) dic x)

  let file_dump contents = [ Command.F contents ] (* FIXME: should check that there is no file_dump in contents *)
end

module EDSL = struct
  include Template

  type command = U.t Command.t

  let input = Workflow.input

  let docker image cmd = Command.Docker (image, cmd)

  let gen_cmd prog_expr ?env ?stdin ?stdout ?stderr args =
    let stdout_expr =
      match stdout with
      | None -> []
      | Some e -> Command.S " > " :: e
    in
    let stdin_expr =
      match stdin with
      | None -> []
      | Some e -> Command.S " < " :: e
    in
    let stderr_expr =
      match stderr with
      | None -> []
      | Some e -> Command.S " 2> " :: e
    in
    let tokens =
      [ prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
      |> List.filter ~f:(( <> ) [])
      |> List.intersperse ~sep:(string " ")
      |> List.concat
    in
    let cmd = Command.Simple_command tokens in
    match env with
    | None -> cmd
    | Some image -> docker image cmd

  let cmd p = gen_cmd [ S p ]

  let internal_cmd subcmd = gen_cmd [ EXE ; S " " ; S subcmd ] ?env:None

  let opt o f x = Command.(S o :: S " " :: f x)

  let opt' o f x = Command.(S o :: S "=" :: f x)

  let flag f x b = if b then f x else []

  let mkdir d = cmd "mkdir" [ d ]

  let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

  let cd p = cmd "cd" [ p ]

  let rm_rf x = cmd "rm" [ string "-rf" ; x ]

  let mv x y = cmd "mv" [ x ; y ]

  let wget ?no_check_certificate ?user ?password ?dest url =
    cmd "wget" [
      option (flag string "--no-check-certificate") no_check_certificate ;
      option (opt "--user" string) user ;
      option (opt "--password" string) password ;
      option (opt "-O" ident) dest ;
      string url ;
    ]

  let ( // ) x y = Command.(x @ [ S "/" ; S y ])

  let or_list xs = Command.Or_list xs
  let and_list xs = Command.And_list xs
  let pipe xs = Command.Pipe_list xs

  let workflow ?descr ?mem ?np ?version cmds =
    let cmd = and_list cmds in
    let deps = Command.deps cmd in
    let action = Exec (Command.map cmd ~f:Workflow.to_dep) in
    Workflow.make ?descr ?mem ?np ?version action deps

  let ( % ) f g x = g (f x)

  let selector x = Selector x
  let ( / ) = Workflow.select

  let docker_image ?tag ?registry ~account ~name () = {
    dck_account = account ;
    dck_name = name ;
    dck_tag = tag ;
    dck_registry = registry ;
  }

end


module Std = struct
  type 'a workflow = 'a Workflow.t
  type nonrec ('a, 'b) selector = ('a, 'b) selector
  type nonrec docker_image = docker_image

  class type ['a,'b] file = object
    method format : 'a
    method encoding : [< `text | `binary] as 'b
  end

  type 'a directory = [`directory of 'a]
  type 'a zip = ([`zip of 'a], [`binary]) file
  type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a bz2 = ([`bz2 of 'a], [`binary]) file constraint 'a = (_,_) #file
  type 'a tar'gz = ([`tar'gz of 'a],[`binary]) file
  type pdf = ([`pdf],[`text]) file
  type html = ([`html], [`text]) file
  type bash_script = ([`bash_script], [`text]) file

  type png = ([`png],[`binary]) file
  type svg = ([`png],[`text]) file

  class type ['a] tabular = object ('a)
    constraint 'a = < header : 'b ; sep : 'c ; comment : 'd ; .. >
    inherit [[`tabular], [`text]] file
    method header : 'b
    method sep : 'c
    method comment : 'd
  end

  class type ['a] tsv = object
    inherit [ < sep : [`tab] ; comment : [`sharp] ; .. > as 'a ] tabular
  end

  module Unix_tools = struct
    open EDSL

    let wget ?descr_url ?no_check_certificate ?user ?password url =
      let info = match descr_url with None -> "" | Some i -> sprintf "(%s)" i in
      workflow ~descr:("utils.wget" ^ info) [
        wget ?no_check_certificate ?user ?password ~dest url
      ]

    let unzip zip =
      workflow ~descr:"utils.unzip" [
        cmd "unzip" [ opt "-d" ident dest ; dep zip ]
      ]

    let gunzip gz =
      workflow ~descr:"utils.gunzip" [
        cmd "gunzip" [ opt "-c" dep gz ] ~stdout:dest
      ]

    let bunzip2 bz2 =
      workflow ~descr:"utils.bunzip2" [
        cmd "bunzip2" [ opt "-c" dep bz2 ] ~stdout:dest
      ]

    let tar_xfz tgz =
      workflow ~descr:"utils.tar_xfz" [
        mkdir_p dest ;
        cmd "tar" [ string "xfz" ; dep tgz ; opt "-C" ident dest ] ;
      ]

    let crlf2lf f =
      workflow ~descr:"utils.crlf2lf" [
        cmd "tr" [ opt "-d" string "'\r'"] ~stdin:(dep f) ~stdout:dest
      ]
  end
end
