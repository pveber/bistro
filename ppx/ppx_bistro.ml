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
    | Antiquot e ->
      let buf = Lexing.from_string e in
      Parse.expression { buf with Lexing.lex_curr_p = loc.Location.loc_start }
  in
  [%expr seq ~sep:"" [%e Ast_convenience.list (List.map f ast)]]

let bistro_mapper argv =
  {
    default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "bistro" ; loc }, pstr)} -> (
          match pstr with
          | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc  = loc;
                                             pexp_desc = Pexp_constant (Pconst_string (sym, _))}, _)}] ->
            Ast_helper.default_loc := loc ;
            [%expr let open Bistro.EDSL in
                         [%e script_of_ast loc (parse sym)] ]
              | _ ->
                raise (Location.Error (
                    Location.error ~loc ("Extension bistro only accepts a string")))
            )
      | x -> default_mapper.expr mapper x
  }

let () = register "bistro" bistro_mapper

