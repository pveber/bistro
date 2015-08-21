open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Asttypes
open Longident

let def_loc txt =
  { txt; loc = !default_loc }

let bistro_mapper argv =
  {
    default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "bistro"; loc }, pstr)} ->
        begin
          match pstr with
          | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc  = loc;
                                             pexp_desc = Pexp_constant (Const_string (sym, _))}, _)}] ->
            Exp.constant ~loc (Const_string (sym, None))
          | _ ->
            raise (Location.Error (
                Location.error ~loc "[%bistro] only accepts a string"))
        end
      | x -> default_mapper.expr mapper x;
  }

let () = register "bistro" bistro_mapper
(* let () = run_main bistro_mapper *)
