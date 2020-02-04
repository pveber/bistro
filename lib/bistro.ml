open Base
open Bistro_internals

type 'a path = Workflow.path

class type directory = object
  method file_kind : [`directory]
end

type 'a workflow = 'a Workflow.t
type 'a pworkflow = 'a path workflow
type 'a dworkflow = < directory ; contents : 'a > path workflow

module Workflow = struct
  include Workflow

  let zip x y =
    pure ~id:"__List.zip__" List.zip_exn $ x $ y

  let fst x = pure ~id:"__fst__" fst $ x
  let snd x = pure ~id:"__snd__" snd $ x

  let spawn2 x y ~f =
    zip x y
    |> spawn ~f:(fun p -> f (fst p) (snd p))

  let eval_paths xs = list (List.map xs ~f:eval_path)
end


module Private = struct
  let reveal x = x
end


class type file = object
  method file_kind : [`regular]
end

class type text_file = object
  inherit file
  method encoding : [`text]
end

class type ['a] sexp_value = object
  inherit file
  method ty : 'a
end

class type binary_file = object
  inherit file
  method encoding : [`binary]
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

class type ['a] zip = object
  inherit binary_file
  method format : [`zip]
  method content_format : 'a
end

class type ['a] gz = object
  constraint 'a = #file
  inherit binary_file
  method format : [`gz]
  method content_format : 'a
end

class type ['a] bz2 = object
  constraint 'a = #file
  inherit binary_file
  method format : [`bz2]
  method content_format : 'a
end

class type ['a] tar = object
  inherit binary_file
  method format : [`tar]
  method content_format : 'a
end

class type bam = object
  inherit binary_file
  method format : [`bam]
end

class type bed3 = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : int
  method f3 : int
end

class type bed4 = object
  inherit bed3
  method f4 : string
end

class type bed5 = object
  inherit bed4
  method f5 : int
end

class type bed6 = object
  inherit bed5
  method f6 : [ `Plus | `Minus | `Unknown ]
end

class type fasta = object
  inherit text_file
  method format : [`fasta]
end

class type indexed_fasta = object
  inherit directory
  method contents : [`indexed_fasta]
end

class type fastq = object
  inherit text_file
  method format : [`fastq]
end

class type sanger_fastq = object
  inherit fastq
  method phred_encoding : [`sanger]
end

class type solexa_fastq = object
  inherit fastq
  method phred_encoding : [`solexa]
end

class type phred64_fastq = object
  inherit fastq
  method phred_encoding : [`phred64]
end

class type gff = object
  inherit tsv
  method header : [`no]
  method f1 : string
  method f2 : string
  method f3 : string
  method f4 : int
  method f5 : int
  method f6 : float
  method f7 : [`Plus | `Minus]
  method f8 : [`frame0 | `frame1 | `frame2]
  method f9 : string
end

class type gff2 = object
  inherit gff
  method version : [`v2]
end

class type gff3 = object
  inherit gff
  method version : [`v3]
end

class type indexed_bam = object
  inherit directory
  method contents : [`indexed_bam]
end

class type sam = object
  inherit text_file
  method format : [`sam]
end

class type sra = object
  inherit binary_file
  method format : [`sra]
end

module Template_dsl = struct
  type template = Workflow.token Template.t

  let dest = [ Template.DEST ]
  let tmp = [ Template.TMP ]
  let np = [ Template.NP ]
  let mem = [ Template.MEM ]

  let string s = [ Template.S s ]
  let int i = string (Int.to_string i)
  let float f = string (Float.to_string f)
  let dep w = [ Template.D (Workflow.Path_token w) ]
  let deps ?quote ~sep w = [ Template.D (Workflow.Path_list_token { elts = w ; quote ; sep }) ]
  let string_dep w = [ Template.D (Workflow.String_token w) ]
  let int_dep w = [ Template.D Workflow.(String_token (app (pure ~id:"__string_of_int__" Int.to_string) w)) ]

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
      | None -> Fn.id
      | Some sep -> List.intersperse ~sep:(string sep)
    in
    List.concat (format xs)

  let enum dic x = string (List.Assoc.find_exn ~equal:Caml.( = ) dic x)

  let file_dump contents = [ Template.F contents ] (* FIXME: should check that there is no file_dump in contents *)
end

module Shell_dsl = struct
  include Template_dsl

  type command = Workflow.shell_command
  type container_image = Command.container_image

  let within_container images cmd = Command.Within_container (images, cmd)

  let gen_cmd prog_expr ?img ?stdin ?stdout ?stderr args =
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
      |> List.filter ~f:(Caml.( <> ) [])
      |> List.intersperse ~sep:(string " ")
      |> List.concat
    in
    let cmd = Command.Simple_command tokens in
    match img with
    | None -> cmd
    | Some image -> within_container image cmd

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

  let docker_image = Command.docker_image

  let bash ?img script = cmd "bash" ?img [ file_dump script ]
end
