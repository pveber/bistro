open Core_kernel.Std

type path = string list
with sexp

let string_of_path = function
  | []
  | "" :: _ -> failwith "string_of_path: wrong path"
  | p -> List.reduce_exn p ~f:Filename.concat

let path_of_string s = String.split ~on:'/' s

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
with sexp

module T = struct
  type u =
    | Input of string * path
    | Select of string * u * path
    | Step of step

  and step = {
    id : string ;
    descr : string ;
    deps : u list ;
    cmd : cmd ;
    np : int ; (** Required number of processors *)
    mem : int ; (** Required memory in MB *)
    timeout : int option ; (** Maximum allowed running time in hours *)
    version : int option ; (** Version number of the wrapper *)
  }

  and cmd =
    | Simple_command of simple_command
    | Run_script of script
    | And_sequence of cmd list
    | Or_sequence of cmd list
    | Pipe_sequence of cmd list

  and simple_command = {
    tokens : token list ;
    env : docker_image option ;
  }

  and script = {
    interpreter : string ;
    args : token list list ;
    text : token list ;
    script_env : docker_image option ;
  }

  and token =
    | S of string
    | D of u
    | DEST
    | TMP
    | NP
    | MEM

  and interpreter = [
    | `bash
    | `ocaml
    | `ocamlscript
    | `perl
    | `python
    | `R
    | `sh
  ]

  with sexp

  type ('a, 'b) selector = Selector of path

end

include T

type 'a workflow = u

let workflow_id = function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id

module Cmd = struct
  type t = cmd

  let deps_of_template tmpl =
    List.filter_map tmpl ~f:(function
        | D r -> Some (r :> u)
        | S _ | DEST | TMP | NP | MEM -> None
      )
    |> List.dedup

  let deps_of_simple_cmd cmd = deps_of_template cmd.tokens
  let rec deps = function
    | And_sequence xs
    | Or_sequence xs
    | Pipe_sequence xs ->
      List.map xs ~f:deps
      |> List.concat
      |> List.dedup
    | Simple_command cmd -> deps_of_simple_cmd cmd
    | Run_script s ->
      deps_of_template s.text
end

module Workflow = struct
  include T
  type 'a t = u

  let id = workflow_id

  let id' = workflow_id

  let input ?(may_change = false) target =
    let hash = if may_change then Some (Digest.file target) else None in
    let id = digest ("input", target, hash) in
    Input (id, path_of_string target)


  let make
      ?(descr = "")
      ?(mem = 100)
      ?(np = 1)
      ?timeout
      ?version
      cmd =
    let deps = Cmd.deps cmd in
    let id = digest ("step", version, cmd) in
    Step { descr ; deps ; cmd ; np ; mem ; timeout ; version ; id }

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
    | Input (_,p) -> (string_of_path p)
    | Select (_, _, p) -> (string_of_path p)
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
  let path p = [ S (string_of_path p) ]
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

  let seq ?(sep = "") xs = List.concat (List.intersperse ~sep:(string sep) xs)

  let enum dic x = [ S (List.Assoc.find_exn dic x) ]

  (* FIXME: remove this?
     let use s = s.tokens *)
end

module EDSL = struct
  include Expr

  type cmd = T.cmd

  let input ?(may_change = false) target =
    let hash = if may_change then Some (Digest.file target) else None in
    let id = digest ("input", target, hash) in
    Input (id, path_of_string target)

  let workflow ?descr ?mem ?np ?timeout ?version cmds =
    let script = match cmds with
      | [] ->
        raise (Invalid_argument "EDSL.workflow: empty script")
      | [ Simple_command _ as cmd ] -> cmd
      | cmds -> And_sequence cmds
    in
    Workflow.make ?descr ?mem ?np ?timeout ?version script


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
    Simple_command { tokens ; env }

  let script interpreter ?env ?stdin ?stdout ?stderr ?(args = []) text =
    Run_script {
      interpreter ;
      script_env = env ;
      args ;
      text ;
    }

  let opt o f x = S o :: S " " :: f x

  let opt' o f x = S o :: S "=" :: f x

  let flag f x b = if b then f x else []

  let mkdir d = cmd "mkdir" [ d ]

  let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

  let cd p = cmd "cd" [ p ]

  let rm_rf x = cmd "rm" [ string "-rf" ; x ]

  let mv x y = cmd "mv" [ x ; y ]

  let wget url ?dest () = cmd "wget" [
      option (opt "-O" ident) dest ;
      string url
    ]

  let ( // ) x y = x @ [ S "/" ; S y ]

  let or_list xs = Or_sequence xs
  let and_list xs = And_sequence xs
  let pipe xs = Pipe_sequence xs

  let with_env vars cmd =
    (
      List.map vars ~f:(fun (var, value) -> [ S var ; S "=" ] @ value)
      |> List.intersperse ~sep:[ S " " ]
      |> List.concat
    )
    @ (S " " :: cmd)

  let ( % ) f g x = g (f x)

  let selector x = Workflow.Selector x
  let ( / ) = Workflow.select

  let docker_image ?tag ?registry ~account ~name () = {
    dck_account = account ;
    dck_name = name ;
    dck_tag = tag ;
    dck_registry = registry ;
  }

end


module Task = struct
  type t = {
    id      : id ;
    descr   : string ;
    deps    : dep list ;
    cmd     : cmd ;
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

  and cmd =
    | Simple_command of simple_command
    | Run_script of script
    | And_sequence of cmd list
    | Or_sequence of cmd list
    | Pipe_sequence of cmd list

  and simple_command = {
    tokens : token list ;
    env : docker_image option ;
  }

  and script = {
    interpreter : string ;
    args : token list list ;
    text : token list ;
    script_env : docker_image option ;
  }

  and template = token list

  and token =
    | S of string
    | D of dep
    | DEST
    | TMP
    | NP
    | MEM
  with sexp

  let denormalize_dep = function
    | Step s -> `Task s.id
    | Input (_, p) -> `Input p
    | Select (_, Input (_, p), q) ->
      `Input (p @ q)
    | Select (_, Step s, p) ->
      `Select (s.id, p)
    | Select (_, Select _, _) -> assert false

  let denormalize_token = function
    | T.S s -> S s
    | T.DEST -> DEST
    | T.TMP -> TMP
    | T.NP -> NP
    | T.MEM -> MEM
    | T.D d -> D (denormalize_dep d)

  let denormalize_template tmpl =
    List.map tmpl ~f:denormalize_token

  let rec denormalize_cmd = function
    | T.Simple_command scmd ->
      let tokens = denormalize_template scmd.tokens in
      Simple_command { tokens ; env = scmd.env }

    | Run_script s ->
      Run_script { text = denormalize_template s.text ;
                   args = List.map s.args ~f:denormalize_template ;
                   interpreter = s.interpreter ;
                   script_env = s.script_env }

    | And_sequence xs -> And_sequence (List.map xs ~f:denormalize_cmd)
    | Or_sequence xs -> And_sequence (List.map xs ~f:denormalize_cmd)
    | Pipe_sequence xs -> And_sequence (List.map xs ~f:denormalize_cmd)

  let rec decompose_workflow_aux accu w =
    let wid = Workflow.id w in
    if String.Map.mem accu wid then
      accu
    else
      match w with
      | T.Input (_, p) -> accu

      | Select (id, dir, p) ->
        decompose_workflow_aux accu dir

      | Step step ->
        let accu = List.fold_left step.deps ~init:accu ~f:decompose_workflow_aux in
        let deps = List.map step.deps ~f:denormalize_dep in
        let t = {
            id = step.id ;
            descr = step.descr ;
            deps ;
            cmd = denormalize_cmd step.cmd ;
            np = step.np ;
            mem = step.mem ;
            version = step.version ;
            timeout = step.timeout ;
          }
        in
        String.Map.add accu ~key:step.id ~data:t

  let classify_workflow = denormalize_dep
  let decompose_workflow w =
    decompose_workflow_aux String.Map.empty w
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

    let wget ?descr_url ?no_check_certificate url =
      let info = match descr_url with None -> "" | Some i -> sprintf "(%s)" i in
      workflow ~descr:("utils.wget" ^ info) [
        cmd "wget" [
          option (flag string "--no-check-certificate") no_check_certificate ;
          opt "-O" ident dest ; string url ]
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
