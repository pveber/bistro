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
  let constr_args args = function
    | [] -> None
    | [x] -> Some x
    | l -> Some (args l)
  let constr s args =
    construct (lid s) (constr_args Exp.tuple args)
  let lid ?loc s = ident (lid ?loc s)
  let nil () = constr "[]" []
  let unit () = constr "()" []
  let tuple = function
    | [] -> unit ()
    | [x] -> x
    | xs -> Exp.tuple xs
  let cons hd tl = constr "::" [hd; tl]
  let list l = List.fold_right ~f:(cons) l ~init:(nil ())
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
            [%expr env#dep [%e Exp.lid (id ^ "_id")]]
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

let rec extract_body = function
  | { pexp_desc = Pexp_fun (_,_,_,body) } -> extract_body body
  | expr -> expr

let rec replace_body new_body = function
  | ({ pexp_desc = Pexp_fun (lab, e1, p, e2) } as expr) ->
    { expr with pexp_desc = Pexp_fun (lab, e1, p, replace_body new_body e2) }
  | _ -> new_body

let rewriter _config _cookies =
  { default_mapper with
    structure_item = fun mapper stri ->
      match stri with
      | { pstr_desc = Pstr_extension (({txt = "bistro"}, payload),
                                      attributes) } -> (
          match payload with
          | PStr [ { pstr_desc =
                       Pstr_value (Nonrecursive,
                                   [ {
                                     pvb_pat = {
                                       ppat_desc = Ppat_var { txt = var }
                                     } as pat ;
                                     pvb_expr = expr ;
                                     pvb_loc = loc  }]) } ] ->
            let body = extract_body expr in
            let deps = ref [] in
            let rewriter = payload_rewriter deps in
            let code = rewriter.expr rewriter body in
            let id = digest code in
            let add_bindings body = List.fold !deps ~init:body ~f:(fun acc (tmpvar, expr) ->
                [%expr
                  let [%p Pat.var (str tmpvar)] = [%e expr] in
                  let [%p Pat.var (str (tmpvar ^ "_id"))] = Bistro.Workflow.to_dep [%e Exp.lid tmpvar] in
                  [%e acc]]
              )
            in
            let dep_list =
              List.map !deps ~f:(fun (v, _) ->
                  [%expr Bistro.Workflow.u [%e Exp.lid v]]
                )
              |> Exp.list
            in
            let descr = var in
            let new_body = [%expr
              let id = [%e Exp.string id] in
              let f env : unit = [%e code] in
              Bistro.Workflow.of_fun
                ~descr:[%e Exp.string descr]
                ~id ~deps:[%e dep_list] f
            ] |> add_bindings
            in
            [%stri let [%p pat] = [%e replace_body new_body expr]]
          | _ -> failwith "bistro extension expects a single expression as payload"
        )

      | _ -> default_mapper.structure_item mapper stri
  }

let () =
  Driver.register
    ~name:"bistro"
    ocaml_version
    rewriter


(* open Ppx_core *)
(* open Ast_builder.Default *)

(* let transformer ~loc ~path expr attrs = *)
(*   eint ~loc 42 *)

(* let _ = *)
(*   let ast_pattern = *)
(*     let open Ast_pattern in *)
(*     pstr ( *)
(*       pstr_value nonrecursive *)
(*         (value_binding ~pat:(ppat_ident __) ~expr:__ ^:: nil) *)
(*       ^:: nil) *)
(*   in *)
(*   Extension.declare "bistro_fun" Extension.Context.structure_item ast_pattern transformer *)

