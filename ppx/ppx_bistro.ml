open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Asttypes
open Longident

let def_loc txt =
  { txt; loc = !default_loc }

let rec primitive_wrapping prim = function
  | { pexp_desc = Pexp_fun (arg_label, opt_expr, arg_pat, body) } as expr ->
    let arg_id = match arg_pat with
      | { ppat_desc = Ppat_var { txt = id } } -> id
      | _ -> assert false
    in
    let wrapped = primitive_wrapping prim body in
    let open Longident in
    Exp.apply
      (Exp.ident (def_loc (Lident "app")))
      [ "n", Exp.constant (Const_string (arg_id, None)) ;
        "", wrapped ;
        "", Exp.ident (def_loc (Lident arg_id)) ]
  | _ -> prim

let bistro_mapper argv =
  {
    default_mapper with
    structure_item = (
      fun mapper structure_item ->
        match structure_item with
        | { pstr_desc =
              Pstr_extension (
                ({ txt = "workflow" ; loc },
                 PStr [
                   { pstr_desc = Pstr_value (Nonrecursive,
                                             [ { pvb_pat = { ppat_desc = Ppat_var { txt = workflow_name ; loc = workflow_name_loc } } as def_pat ;
                                                 pvb_expr = def_expr } as value_binding ]) } ]),
                attributes) ;
            pstr_loc } ->
          let wrapped_primitive = primitive_wrapping (Exp.ident (def_loc (Lident "prim"))) def_expr in
          let full_expr = Exp.let_ Nonrecursive [ Vb.mk (Pat.var (def_loc "prim")) def_expr ] wrapped_primitive in
          let value_binding = { value_binding with
                                pvb_pat = { def_pat with ppat_desc = Ppat_var { txt = workflow_name ^workflow_name ; loc = workflow_name_loc } } ;
                                pvb_expr = full_expr } in
          { pstr_desc = Pstr_value (Nonrecursive, [value_binding]) ; pstr_loc }
        | _ -> assert false
    )
  }

let () = register "bistro" bistro_mapper
(* let () = run_main bistro_mapper *)
