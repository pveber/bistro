open Core_kernel
open Bistro_base

type docker_image = Command.docker_image = {
  dck_account : string ;
  dck_name : string ;
  dck_tag : string option ;
  dck_registry : string option ;
}
[@@deriving sexp]

type ('a, 'b) selector = Selector of string list

type 'a workflow = 'a Workflow.t
type shell_command = Workflow.shell_command
let input = Workflow.input
let shell = Workflow.shell
let select dir (Selector path) = Workflow.select dir path
let selector x = Selector x
let ( /> ) = select

type any_workflow = Any_workflow : _ workflow -> any_workflow


module Template_dsl = struct
  type template = Workflow.template

  let dest = [ Template.DEST ]
  let tmp = [ Template.TMP ]
  let np = [ Template.NP ]
  let mem = [ Template.MEM ]

  let string s = [ Template.S s ]
  let int i = string (string_of_int i)
  let float f = string (Float.to_string f)
  let dep w = [ Template.D (Workflow.Any w) ]

  let quote ?using:(c = '"') e =
    let quote_symbol = Template.S (Char.to_string c) in
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

  let file_dump contents = [ Template.F contents ] (* FIXME: should check that there is no file_dump in contents *)
end

module Shell_dsl = struct
  include Template_dsl

  type command = Workflow.shell_command

  let docker image cmd = Command.Docker (image, cmd)

  let gen_cmd prog_expr ?env ?stdin ?stdout ?stderr args =
    let stdout_expr =
      match stdout with
      | None -> []
      | Some e -> Template.S " > " :: e
    in
    let stdin_expr =
      match stdin with
      | None -> []
      | Some e -> Template.S " < " :: e
    in
    let stderr_expr =
      match stderr with
      | None -> []
      | Some e -> Template.S " 2> " :: e
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

  let opt o f x = Template.(S o :: S " " :: f x)

  let opt' o f x = Template.(S o :: S "=" :: f x)

  let flag f x b = if b then f x else []

  let mkdir d = cmd "mkdir" [ d ]

  let mkdir_p d = cmd "mkdir" [ string "-p" ; d ]

  let cd p = cmd "cd" [ p ]

  let rm_rf x = cmd "rm" [ string "-rf" ; x ]

  let mv x y = cmd "mv" [ x ; y ]

  let ( // ) x y = Template.(x @ [ S "/" ; S y ])

  let or_list xs = Command.Or_list xs
  let and_list xs = Command.And_list xs
  let pipe xs = Command.Pipe_list xs


  let ( % ) f g x = g (f x)

  let docker_image ?tag ?registry ~account ~name () = {
    dck_account = account ;
    dck_name = name ;
    dck_tag = tag ;
    dck_registry = registry ;
  }

end


module File_formats = struct
  (** Conventional type to represent directory targets *)
  class type ['a] directory = object
    method file_type : [`directory]
    method contents : 'a
  end

  (** Conventional type to represent file targets. The object type is to
      represent properties of the file, like the type of encoding (text
      or binary) or the format. *)
  class type file = object
    method file_type : [`regular]
  end

  class type binary_file = object
    inherit file
    method encoding : [`binary]
  end

  (** Conventional type to represent OCaml values saved with the
      {!module:Marshal} module. *)
  class type ['a] value = object
    inherit binary_file
    method format : [`marshalled_value]
    method content_type : 'a
  end

  class type text_file = object
    inherit file
    method encoding : [`text]
  end

  class type ['a] sexp_value = object
    inherit text_file
    method format : [`sexp_value]
    method content_type : 'a
  end

  class type pdf = object
    inherit text_file
    method format : [`pdf]
  end

  class type html = object
    inherit text_file
    method format : [`html]
  end

  class type png = object
    inherit binary_file
    method format : [`png]
  end

  class type svg = object
    inherit text_file
    method format : [`svg]
  end

  class type tsv = object
    inherit text_file
    method colum_separator : [`tab]
  end

end

include File_formats

module Private = struct
  let reveal (x : 'a workflow) = (x : 'a Workflow.t)
end
