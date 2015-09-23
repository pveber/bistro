open Ast_mapper
(* open Ast_helper *)
open Asttypes
open Parsetree
open Asttypes
open Longident

type ast = item list
and item =
  | Text of string
  | Antiquot of string

let parse =
  let re = Str.regexp "{{\\([^{]\\|{[^{]\\)*}}" in
  let f = function
    | Str.Text s -> Text s
    | Str.Delim s -> Antiquot (String.sub s 2 (String.length s - 4))
  in
  fun s ->
    Str.full_split re s
    |> List.map f

let script_of_ast loc ast =
  let f = function
    | Text s -> [%expr string [%e Ast_convenience.str s] ]
    | Antiquot "DEST" -> [%expr dest ]
    | Antiquot "TMP" -> [%expr tmp ]
    | Antiquot e ->
      let buf = Lexing.from_string e in
      Parse.expression { buf with Lexing.lex_curr_p = loc.Location.loc_start }
  in
  Ast_convenience.list (List.map f ast)

let expr_of_interpreter = function
  | `sh -> [%expr `sh]
  | `bash -> [%expr `bash]
  | `ocaml -> [%expr `ocaml]
  | `ocamlscript -> [%expr `ocamlscript]
  | `python -> [%expr `python]
  | `R -> [%expr `R]
  | `perl -> [%expr `perl]

let interpreter_of_string = function
  | "sh" -> Some `sh
  | "bash" -> Some `bash
  | "ocaml" -> Some `ocaml
  | "ocamlscript" -> Some `ocamlscript
  | "R" -> Some `R
  | "python" -> Some `python
  | "perl" -> Some `perl
  | _ -> None

let bistro_mapper argv =
  {
    default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = ext_name; loc }, pstr)} as x -> (
          match interpreter_of_string ext_name with
          | Some interpreter -> (
              match pstr with
              | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc  = loc;
                                                 pexp_desc = Pexp_constant (Const_string (sym, _))}, _)}] ->
                Ast_helper.default_loc := loc ;
                [%expr let open Bistro.EDSL in
                       Bistro.Script.make
                         [%e expr_of_interpreter interpreter]
                         [%e script_of_ast loc (parse sym)] ]
              | _ ->
                raise (Location.Error (
                    Location.error ~loc ("Extension " ^ ext_name ^ " only accepts a string")))
            )
          | None -> default_mapper.expr mapper x
        )
      | x -> default_mapper.expr mapper x
  }

let () = register "bistro" bistro_mapper

