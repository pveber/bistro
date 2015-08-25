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

let script_of_ast ast =
  let f = function
    | Text s -> [%expr string [%e Ast_convenience.str s] ]
    | Antiquot "DEST" -> [%expr dest ]
    | Antiquot "TMP" -> [%expr tmp ]
    | Antiquot e -> Parse.expression (Lexing.from_string e)
  in
  Ast_convenience.list (List.map f ast)

let string_starts_with u v =
  if String.length u < String.length v then false
  else (
    String.sub u 0 (String.length v) = v
  )

let interpreter_of_ext ext_name =
  let k = String.index ext_name '.' in
  match String.sub ext_name (k + 1) (String.length ext_name - k - 1) with
  | "sh" -> [%expr `sh]
  | "ocaml" -> [%expr `ocaml]
  | "ocamlscript" -> [%expr `ocamlscript]
  | s -> failwith ("No interpreter for extension: " ^ ext_name)

let bistro_mapper argv =
  {
    default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = ext_name; loc }, pstr)}
        when string_starts_with ext_name "bistro." ->
        begin
          match pstr with
          | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc  = loc;
                                             pexp_desc = Pexp_constant (Const_string (sym, _))}, _)}] ->
            Ast_helper.default_loc := loc ;
            [%expr let open Bistro.Script in make [%e interpreter_of_ext ext_name ] [%e script_of_ast (parse sym)] ]
          | _ ->
            raise (Location.Error (
                Location.error ~loc "[%bistro] only accepts a string"))
        end
      | x -> default_mapper.expr mapper x;
  }

let () = register "bistro" bistro_mapper

