open Core_kernel.Std

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

let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let ( % ) f g x = g (f x)

let ok = function
  | `Ok x -> x
  | `Error e -> raise e

module Utils = struct
  let python_version fmt =
    let regexp = match fmt with
      | `M_m -> "[0-9]\\.[0-9]"
    in
    let ic = Unix.open_process_in (sprintf "python --version 2>&1 | grep -o '%s'" regexp) in
    let r = In_channel.input_line ic in
    In_channel.close ic ;
    r
end

type docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

module T = struct
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
    timeout : int option ; (** Maximum allowed running time in hours *)
    version : int option ; (** Version number of the wrapper *)
    precious : bool ;
  }

  and action =
    | Command : command -> action
    | Eval : _ expr -> action

  and command =
    | Docker of docker_image * command
    | Simple_command of token list
    | And_list of command list
    | Or_list of command list
    | Pipe_list of command list

  and dump = {
    dest : token list ;
    contents : token list ;
    for_container : bool ;
  }

  and token =
    | S of string
    | D of u
    | F of token list
    | DEST
    | TMP
    | NP
    | MEM

  and _ expr =
    | E_primitive : { id : string ; value : 'a } -> 'a expr
    | E_app : ('a -> 'b) expr * 'a expr -> 'b expr
    | E_dest : string expr
    | E_tmp : string expr
    | E_np : int expr
    | E_mem : int expr
    | E_dep : u -> string expr

  type ('a, 'b) selector = Selector of Path.t

end

include T

type 'a workflow = u

type any_workflow = Workflow : _ workflow -> any_workflow

let workflow_id = function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id

module Cmd = struct
  type t = command

  let rec deps_of_template tmpl =
    List.map tmpl ~f:(function
        | D r -> [ r ]
        | F toks -> deps_of_template toks
        | S _ | DEST | TMP | NP | MEM -> []
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
end

let rec expr_deps : type s. s expr -> u list = function
  | E_primitive _ -> []
  | E_app (x, f) ->
    List.dedup (expr_deps x @ expr_deps f)
  | E_dep u -> [ u ]
  | E_dest -> []
  | E_tmp -> []
  | E_np -> []
  | E_mem -> []

module Workflow = struct
  include T
  type 'a t = u

  let id = workflow_id

  let id' = workflow_id

  let input ?(may_change = false) target =
    let hash = if may_change then Some (Digest.file target) else None in
    let id = digest ("input", target, hash) in

    Input (id, Path.make_relative target)

  let rec digestable_command = function
    | Docker (im, cmd) -> `Docker (im, digestable_command cmd)
    | Simple_command toks ->
      `Simple_command (List.map ~f:digestable_token toks)
    | And_list cmds -> `And_list (List.map cmds ~f:digestable_command)
    | Or_list cmds -> `Or_list (List.map cmds ~f:digestable_command)
    | Pipe_list cmds -> `Pipe_list (List.map cmds ~f:digestable_command)

  and digestable_token = function
    | S s -> `S s
    | D (Input (id, _)) -> `D (`Input id)
    | D (Select (id, _, _)) -> `D (`Select id)
    | D (Step s) -> `D (`Step s.id)
    | F toks -> `F (List.map toks ~f:digestable_token)
    | DEST -> `DEST
    | TMP -> `TMP
    | NP -> `NP
    | MEM -> `MEM

  let rec digestable_expr : type s. s expr -> _ = function
    | E_primitive { id } -> `Primitive id
    | E_app (x, f) -> `App (digestable_expr x, digestable_expr f)
    | E_dep (Input (id, _)) -> `Dep (`Input id)
    | E_dep (Select (id, _, _)) -> `Dep (`Select id)
    | E_dep (Step { id }) -> `Dep (`Step id)
    | E_dest -> `DEST
    | E_tmp -> `TMP
    | E_np -> `NP
    | E_mem -> `MEM

  let digestable_action = function
    | Command cmd -> digestable_command cmd
    | Eval expr -> digestable_expr expr

  let deps_of_action = function
    | Command cmd -> Cmd.deps cmd
    | Eval expr -> expr_deps expr

  let make
      ?(descr = "")
      ?(mem = 100)
      ?(np = 1)
      ?timeout
      ?version
      ?(precious = false)
      action =
    let deps = deps_of_action action in
    let id = digest ("step", version, digestable_action action) in
    Step { descr ; deps ; action ; np ; mem ; timeout ; version ; id ; precious }

  let select u (Selector path) =
    let u, path =
      match u with
      | Select (_, v, p) -> v, p @ path
      | Input _ | Step _ -> u, path
    in
    let id = digest ("select", id u, path) in
    Select (id, u, path)

  let rec collect accu u =
    let accu' = List.Assoc.add accu (id u) u in
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

type 'a directory = [`directory of 'a]

module Expr = struct
  type t = token list

  let dest = [ DEST ]
  let tmp = [ TMP ]
  let np = [ NP ]
  let mem = [ MEM ]

  let string s = [ S s ]
  let int i = [ S (string_of_int i) ]
  let float f = [ S (Float.to_string f) ]
  let path p = [ S (Path.to_string p) ]
  let dep w = [ D w ]

  let quote ?(using = '"') e =
    let quote_symbol = Char.to_string using in
    S quote_symbol :: e @ [ S quote_symbol ]

  let option f = function
    | None -> []
    | Some x -> f x

  let list f ?(sep = ",") l =
    List.map l ~f
    |> List.intersperse ~sep:[ S sep ]
    |> List.concat

  let seq ?sep xs =
    let format = match sep with
      | None -> ident
      | Some sep -> List.intersperse ~sep:(string sep)
    in
    List.concat (format xs)

  let enum dic x = [ S (List.Assoc.find_exn dic x) ]

  let file_dump contents = [ F contents ] (* FIXME: should check that there is no file_dump in contents *)

  (* FIXME: remove this?
     let use s = s.tokens *)
end

module EDSL = struct
  include Expr

  let input = Workflow.input

  let docker image cmd = Docker (image, cmd)

  let cmd p ?env ?stdin ?stdout ?stderr args =
    let prog_expr = [ S p ] in
    let stdout_expr =
      match stdout with
      | None -> []
      | Some e -> S " > " :: e
    in
    let stdin_expr =
      match stdin with
      | None -> []
      | Some e -> S " < " :: e
    in
    let stderr_expr =
      match stderr with
      | None -> []
      | Some e -> S " 2> " :: e
    in
    let tokens =
      [ prog_expr ] @ args @ [ stdin_expr ; stdout_expr ; stderr_expr ]
      |> List.filter ~f:(( <> ) [])
      |> List.intersperse ~sep:[S " "]
      |> List.concat
    in
    let cmd = Simple_command tokens in
    match env with
    | None -> cmd
    | Some image -> docker image cmd

  let opt o f x = S o :: S " " :: f x

  let opt' o f x = S o :: S "=" :: f x

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

  let ( // ) x y = x @ [ S "/" ; S y ]

  let or_list xs = Or_list xs
  let and_list xs = And_list xs
  let pipe xs = Pipe_list xs

  let workflow ?descr ?mem ?np ?timeout ?version cmds =
    Workflow.make ?descr ?mem ?np ?timeout ?version (Command (and_list cmds))

  let ( % ) f g x = g (f x)

  let selector x = Workflow.Selector x
  let ( / ) = Workflow.select

  let docker_image ?tag ?registry ~account ~name () = {
    dck_account = account ;
    dck_name = name ;
    dck_tag = tag ;
    dck_registry = registry ;
  }

  let precious = function
    | (Input _ | Select _ as w) -> w
    | Step s -> Step { s with precious = true }
end



module Std = struct
  type 'a workflow = 'a Workflow.t
  type ('a, 'b) selector = ('a, 'b) Workflow.selector

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
