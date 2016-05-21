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

let docker_image ?tag ?registry ~account ~name () = {
  dck_account = account ;
  dck_name = name ;
  dck_tag = tag ;
  dck_registry = registry ;
}

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
    | And_sequence of cmd list
    | Or_sequence of cmd list
    | Pipe_sequence of cmd list

  and simple_command = {
    tokens : token list ;
    env : docker_image option ;
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

end

include T

let workflow_id = function
  | Input (id, _)
  | Select (id, _, _)
  | Step { id } -> id

module Cmd = struct
  type t = cmd

  let deps_of_simple_cmd cmd =
    List.filter_map cmd.tokens ~f:(function
        | D r -> Some (r :> u)
        | S _ | DEST | TMP | NP | MEM -> None
      )
    |> List.dedup

  let rec deps = function
    | And_sequence xs
    | Or_sequence xs
    | Pipe_sequence xs ->
      List.map xs ~f:deps
      |> List.concat
      |> List.dedup
    | Simple_command cmd -> deps_of_simple_cmd cmd

  let string_of_token ~string_of_workflow ~tmp ~dest ~np ~mem = function
    | S s -> s
    | D w -> string_of_workflow (w :> u)
    | DEST -> dest
    | TMP -> tmp
    | NP -> string_of_int np
    | MEM -> string_of_int mem

  let rec string_of_simple_cmd ~use_docker ~string_of_workflow ~tmp ~dest ~np ~mem cmd =
    match use_docker, cmd.env with
    | true, Some image ->
      let image_dest = "/bistro/dest" in
      let image_tmp = "/bistro/tmp" in
      let image_dep d = sprintf "/bistro/data/%s" (workflow_id d) in
      let image_cmd =
        string_of_simple_cmd
          ~use_docker:false
          ~string_of_workflow:image_dep
          ~tmp:image_tmp
          ~dest:image_dest
          ~np ~mem
          cmd
      in
      let deps = deps_of_simple_cmd cmd in
      let deps_mount =
        let f d =
          sprintf
            "-v %s:%s"
            (string_of_workflow d)
            (image_dep d)
        in
        List.map deps ~f
        |> String.concat ~sep:" "
      in
      let tmp_mount = sprintf "-v %s:%s" tmp image_tmp in
      let dest_mount = sprintf "-v %s:%s" Filename.(dirname dest) Filename.(dirname image_dest) in
      let image =
        sprintf "%s%s/%s%s"
          (Option.value_map ~default:"" ~f:(sprintf "%s/") image.dck_registry)
          image.dck_account
          image.dck_name
          (Option.value_map ~default:"" ~f:(sprintf ":%s")  image.dck_tag)
      in
      sprintf
        "docker run %s %s %s -t %s sh -c \"%s\""
        deps_mount
        tmp_mount
        dest_mount
        image
        image_cmd
    | _ ->
      let f = string_of_token ~string_of_workflow ~tmp ~dest ~np ~mem in
      List.map cmd.tokens ~f
      |> String.concat

  let rec to_string ~use_docker ~string_of_workflow ~tmp ~dest ~np ~mem cmd =
    let par x = "(" ^ x ^ ")" in
    let par_if_necessary x y = match x with
      | Simple_command _ -> y
      | And_sequence _
      | Or_sequence _
      | Pipe_sequence _ -> par y
    in
    let f sep xs =
      List.map xs ~f:(to_string ~use_docker ~string_of_workflow ~tmp ~dest ~np ~mem)
      |> List.map2_exn ~f:par_if_necessary xs
      |> String.concat ~sep
    in
    match cmd with
    | And_sequence xs -> f " && " xs
    | Or_sequence xs -> f " || " xs
    | Pipe_sequence xs -> f " | " xs
    | Simple_command cmd ->
      string_of_simple_cmd ~use_docker ~string_of_workflow ~tmp ~dest ~np ~mem cmd
end

module Workflow = struct
  include T
  type 'a t = u
  type ('a, 'b) selector = Selector of path

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
    let script_as_string : string =
      Cmd.to_string
        ~use_docker:true
        ~string_of_workflow:id
        ~np:1
        ~mem:1024
        ~tmp:"TMP"
        ~dest:"DEST"
        cmd
    in
    let id = digest ("step", version, script_as_string) in
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
