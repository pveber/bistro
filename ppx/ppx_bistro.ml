open Base
open Migrate_parsetree
open Ast_404
open Ast_mapper
open Asttypes
open Parsetree
open Ast_helper

let lid ?(loc= !default_loc) str =
  Location.mkloc (Longident.parse str) loc

let str ?(loc= !default_loc) str =
  Location.mkloc str loc

module Exp = struct
  include Exp
  let string s = constant (Const.string s)
  let lid ?loc s = ident (lid ?loc s)
end

let ( += ) r v = r := v :: !r

let digest x =
  Caml.Digest.to_hex (Caml.Digest.string (Caml.Marshal.to_string x []))

let ocaml_version = Versions.ocaml_404

let new_id =
  let c = ref 0 in
  fun () -> "v" ^ (Int.to_string !c)

let payload_rewriter acc =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({txt = "dep"}, payload) ; pexp_loc = loc } -> (
          match payload with
          | PStr [ { pstr_desc = Pstr_eval (e, _) ; pstr_loc = loc } ] ->
            let id = new_id () in
            acc += (id, e) ;
            [%expr env#dep (`Dep [%e Exp.lid (id ^ "_id")])]
          | _ -> failwith "expected a workflow expression"
        )
      | { pexp_desc = Pexp_extension ({txt = ("dest" | "np" | "mem" | "tmp" as ext)}, payload) ; pexp_loc = loc } -> (
          match payload with
          | PStr [] -> (
              match ext with
              | "dest" -> [%expr env#dest]
              | "tmp" -> [%expr env#tmp]
              | "np" -> [%expr env#np]
              | "mem" -> [%expr env#mem]
              | _ -> assert false
            )
          | _ -> failwith "expected empty payload"
        )
      | _ -> default_mapper.expr mapper expr
  }

let rewriter _config _cookies =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({txt = "bistro_fun"}, payload) } -> (
          match payload with
          | PStr [ { pstr_desc = Pstr_eval (expr, _) ; pstr_loc = loc } ] ->
            let deps = ref [] in
            let rewriter = payload_rewriter deps in
            let code = rewriter.expr rewriter expr in
            let id = digest code in
            let add_bindings body = List.fold !deps ~init:body ~f:(fun acc (tmpvar, expr) ->
                [%expr
                  let [%p Pat.var (str tmpvar)] = [%e expr] in
                  let [%p Pat.var (str (tmpvar ^ "_id"))] = Bistro.Workflow.id [%e Exp.lid tmpvar] in
                  [%e acc]]
              )
            in
            [%expr
              let id = [%e Exp.string id] in
              let f env = [%e code] in
              (id, f) ]
            |> add_bindings
          | _ -> failwith "bistro_fun extension expects a single expression as payload"
        )

      | _ -> default_mapper.expr mapper expr
  }

let () =
  Driver.register
    ~name:"bistro_fun"
    ocaml_version
    rewriter


(* open Ppx_core *)
(* open Ast_builder.Default *)

(* let transformer ~loc ~path expr attrs = *)
(*   eint ~loc 42 *)

(* let _ = *)
(*   let ast_pattern = Ast_pattern.(pstr ((pstr_eval __ __) ^:: nil)) in *)
(*   Extension.declare "bistro_fun" Extension.Context.expression ast_pattern transformer *)

(* open Ast_mapper *)
(* (\* open Ast_helper *\) *)
(* open Asttypes *)
(* open Parsetree *)
(* open Asttypes *)
(* open Longident *)

(* type ast = item list *)
(* and item = *)
(*   | Text of string *)
(*   | Antiquot of string *)

(* let parse = *)
(*   let re = Str.regexp "{{\\([^{]\\|{[^{]\\)*}}" in *)
(*   let f = function *)
(*     | Str.Text s -> Text s *)
(*     | Str.Delim s -> Antiquot (String.sub s 2 (String.length s - 4)) *)
(*   in *)
(*   fun s -> *)
(*     Str.full_split re s *)
(*     |> List.map f *)

(* let script_of_ast loc ast = *)
(*   let f = function *)
(*     | Text s -> [%expr string [%e Ast_convenience.str s] ] *)
(*     | Antiquot e -> *)
(*       let buf = Lexing.from_string e in *)
(*       Parse.expression { buf with Lexing.lex_curr_p = loc.Location.loc_start } *)
(*   in *)
(*   [%expr seq ~sep:"" [%e Ast_convenience.list (List.map f ast)]] *)

(* let bistro_mapper argv = *)
(*   { *)
(*     default_mapper with *)
(*     expr = fun mapper expr -> *)
(*       match expr with *)
(*       | { pexp_desc = Pexp_extension ({ txt = "bistro" ; loc }, pstr)} -> ( *)
(*           match pstr with *)
(*           | PStr [{ pstr_desc = Pstr_eval ({ pexp_loc  = loc; *)
(*                                              pexp_desc = Pexp_constant (Pconst_string (sym, _))}, _)}] -> *)
(*             Ast_helper.default_loc := loc ; *)
(*             [%expr let open Bistro.EDSL in *)
(*                          [%e script_of_ast loc (parse sym)] ] *)
(*               | _ -> *)
(*                 raise (Location.Error ( *)
(*                     Location.error ~loc ("Extension bistro only accepts a string"))) *)
(*             ) *)
(*       | x -> default_mapper.expr mapper x *)
(*   } *)

(* let () = register "bistro" bistro_mapper *)
