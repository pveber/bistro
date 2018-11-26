open Base
open Ppxlib

let rec extract_body = function
  | { pexp_desc = Pexp_fun (_,_,_,body) ; _ } -> extract_body body
  | { pexp_desc = Pexp_constraint (expr, ty) ; _ } -> expr, Some ty
  | expr -> expr, None

let rec replace_body new_body = function
  | ({ pexp_desc = Pexp_fun (lab, e1, p, e2) ; _ } as expr) ->
    { expr with pexp_desc = Pexp_fun (lab, e1, p, replace_body new_body e2) }
  | _ -> new_body

let digest x =
  Caml.Digest.to_hex (Caml.Digest.string (Caml.Marshal.to_string x []))

let string_of_expression e =
  let buf = Buffer.create 251 in
  Pprintast.expression (Caml.Format.formatter_of_buffer buf) e ;
  Buffer.contents buf

let new_id =
  let c = ref 0 in
  fun () -> Caml.incr c ; Printf.sprintf "__v%d__" !c

module B = struct
  include Ast_builder.Make(struct let loc = Location.none end)
  let elident v = pexp_ident (Located.lident v)
  let econstr s args =
    let args = match args with
      | [] -> None
      | [x] -> Some x
      | l -> Some (pexp_tuple l)
    in
    pexp_construct (Located.lident s) args
  let enil () = econstr "[]" []
  let econs hd tl = econstr "::" [hd; tl]
  let elist l = List.fold_right ~f:econs l ~init:(enil ())
  let pvar v = ppat_var (Located.mk v)
end

type insert_type =
  | Workflow
  | Expr
  | Path_workflow

let insert_type_of_ext = function
  | "workflow" -> Workflow
  | "eval" -> Expr
  | "path" -> Path_workflow
  | ext -> failwith ("Unknown insert " ^ ext)

class payload_rewriter = object
  inherit [(string * expression * insert_type) list] Ast_traverse.fold_map as super
  method! expression expr acc =
    match expr with
    | { pexp_desc = Pexp_extension ({txt = ext ; _}, payload) ; _ } -> (
        match payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _) ; _ } ] ->
          let id = new_id () in
          let acc' = (id, e, insert_type_of_ext ext) :: acc in
          let expr' = B.elident id in
          expr', acc'
        | _ -> failwith "expected an expression"
      )
    | _ -> super#expression expr acc

end

let rewriter ~loc ~path:_ (* descr version mem np *) expr =
  (* let np = Option.value np ~default:(B.eint 1) in *)
  (* let mem = Option.value mem ~default:(B.eint 100) in *)
  (* let descr = Option.value descr ~default:(B.estring var) in *)
  (* let version = Option.value version ~default:(B.eint 0) in *)
  (* let body, body_type = extract_body expr in *)
  let rewriter = new payload_rewriter in
  let code, deps = rewriter#expression expr [] in
  let id = digest (string_of_expression code) in
  let add_renamings init =
    List.fold deps ~init ~f:(fun acc (tmpvar, expr, ext) ->
        let rhs = match ext with
          | Workflow      -> [%expr Bistro.Expr.eval_workflow [%e expr]]
          | Path_workflow -> [%expr Bistro.Expr.eval_path [%e expr]]
          | Expr          -> expr
        in
        [%expr let [%p B.pvar tmpvar] = [%e rhs] in [%e acc]]
      )
  in
  match deps with
  | [] ->
    [%expr Bistro.Expr.pure ~id:[%e B.estring id] [%e code]]
  | (h_tmpvar, _, _) :: t ->
    let tuple_expr =
      List.fold_right t ~init:(B.elident h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          [%expr Bistro.Expr.both [%e B.elident tmpvar] [%e acc]]
        )
    in
    let tuple_pat =
      List.fold_right t ~init:(B.pvar h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          Ast_builder.Default.ppat_tuple ~loc [B.pvar tmpvar; acc]
        )
    in
    [%expr
      Bistro.Expr.app
        (Bistro.Expr.pure ~id:[%e B.estring id] (fun [%p tuple_pat] -> [%e code]))
        [%e tuple_expr]]
    |> add_renamings

(* let code_with_arguments = *)
  (*   List.fold *)
  (*     deps *)
  (*     ~init:[%expr fun env -> [%e code]] *)
  (*     ~f:(fun acc (tmpvar, _) -> *)
  (*         [%expr fun [%p B.pvar tmpvar] -> [%e acc]] *)
  (*       ) *)
  (* in *)
  (* let id = digest (string_of_expression code) in *)
  (* let workflow_expr = *)
  (*   List.fold_right *)
  (*     deps *)
  (*     ~init:[%expr Bistro.Expr.pure ~id f] *)
  (*     ~f:(fun (tmpvar, payload) acc -> *)
  (*         let arg = match payload with *)
  (*           | Param _ -> *)
  (*             [%expr Bistro.Expr.(pure_data [%e B.elident tmpvar])] *)
  (*           | Dep _  -> *)
  (*             [%expr Bistro.Expr.(dep (pureW [%e B.elident tmpvar]))] *)
  (*           | Deps _ -> *)
  (*             [%expr Bistro.Expr.(deps [%e B.elident tmpvar])] *)
  (*         in *)
  (*         [%expr Bistro.Expr.app [%e acc] [%e arg]] *)
  (*       ) *)
  (* in *)
  (* let add_bindings body = List.fold deps ~init:body ~f:(fun acc (tmpvar, payload) -> *)
  (*     match payload with *)
  (*     | Param expr *)
  (*     | Dep expr *)
  (*     | Deps expr -> *)
  (*       [%expr *)
  (*         let [%p B.pvar tmpvar] = [%e expr] in *)
  (*         [%e acc]] *)
  (*   ) *)
  (* in *)
  (* let new_body = *)
  (*   [%expr *)
  (*     let id = [%e B.estring id] in *)
  (*     let f = [%e code_with_arguments] in *)
  (*     let expr = [%e workflow_expr] in *)
  (*     Bistro.Private.closure *)
  (*       (\* ~descr:[%e descr] *\) *)
  (*       (\* ~np:[%e np] *\) *)
  (*       (\* ~mem:[%e mem] *\) *)
  (*       (\* ~version:[%e version] *\) *)
  (*       expr *)
  (*   ] *)
  (*   |> add_bindings *)
  (*   |> (fun body -> *)
  (*       match body_type with *)
  (*       | None -> body *)
  (*       | Some ty -> [%expr ([%e body] : [%t ty])] *)
  (*     ) *)
  (* in *)
  (* [%stri let [%p B.pvar var] = [%e replace_body new_body expr]] *)

(* let np_attr = *)
(*   Attribute.declare "bistro.np" *)
(*     Attribute.Context.value_binding *)
(*     Ast_pattern.(single_expr_payload (__)) *)
(*     (fun x -> x) *)

(* let mem_attr = *)
(*   Attribute.declare "bistro.mem" *)
(*     Attribute.Context.value_binding *)
(*     Ast_pattern.(single_expr_payload (__)) *)
(*     (fun x -> x) *)

(* let descr_attr = *)
(*   Attribute.declare "bistro.descr" *)
(*     Attribute.Context.value_binding *)
(*     Ast_pattern.(single_expr_payload (__)) *)
(*     (fun x -> x) *)

(* let version_attr = *)
(*   Attribute.declare "bistro.version" *)
(*     Attribute.Context.value_binding *)
(*     Ast_pattern.(single_expr_payload (__)) *)
(*     (fun x -> x) *)

let ext =
  let open Extension in
  declare "bistro" Context.expression Ast_pattern.(single_expr_payload __) rewriter

let () =
  Driver.register_transformation "bistro" ~extensions:[ ext ]
