open Base
module L = Location
open Ppxlib

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
  let enone () = econstr "None" []
  let esome x = econstr "Some" [ x ]
  let eopt x = match x with
    | None -> enone ()
    | Some x -> esome x
  let elist l = List.fold_right ~f:econs l ~init:(enil ())
  let pvar v = ppat_var (Located.mk v)
end

type insert_type =
  | Value
  | Path
  | Param

let insert_type_of_ext = function
  | "eval"  -> Value
  | "path"  -> Path
  | "param" -> Param
  | ext -> failwith ("Unknown insert " ^ ext)

class payload_rewriter = object
  inherit [(string * expression * insert_type) list] Ast_traverse.fold_map as super
  method! expression expr acc =
    match expr with
    | { pexp_desc = Pexp_extension ({txt = ("dest" | "np" | "mem" | "tmp" as ext) ; _ }, payload) ; pexp_loc = loc ; _ } -> (
        match payload with
        | PStr [] -> (
            let expr' = match ext with
              | "dest" -> [%expr __dest__]
              (* | "tmp" -> [%expr env#tmp] *)
              (* | "np" -> [%expr env#np] *)
              (* | "mem" -> [%expr env#mem] *)
              | _ -> assert false
            in
            expr', acc
          )
        | _ -> failwith "expected empty payload"

      )
    | { pexp_desc = Pexp_extension ({txt = ("eval" | "path" | "param" as ext) ; loc ; _}, payload) ; _ } -> (
        match payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _) ; _ } ] ->
          let id = new_id () in
          let acc' = (id, e, insert_type_of_ext ext) :: acc in
          let expr' = B.elident id in
          expr', acc'
        | _ -> failwith (Location.raise_errorf ~loc "expected an expression")
      )
    | _ -> super#expression expr acc

end

let add_renamings ~loc deps init =
  List.fold deps ~init ~f:(fun acc (tmpvar, expr, ext) ->
      let rhs = match ext with
        | Path  -> [%expr Bistro.Workflow.eval_path [%e expr]]
        | Param -> [%expr Bistro.Workflow.pure_data [%e expr]]
        | Value -> expr
      in
      [%expr let [%p B.pvar tmpvar] = [%e rhs] in [%e acc]]
    )

let build_applicative ~loc deps code =
  let id = digest (string_of_expression code) in
  match deps with
  | [] ->
    [%expr Bistro.Workflow.pure ~id:[%e B.estring id] [%e code]]
  | (h_tmpvar, _, _) :: t ->
    let tuple_expr =
      List.fold_right t ~init:(B.elident h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          [%expr Bistro.Workflow.both [%e B.elident tmpvar] [%e acc]]
        )
    in
    let tuple_pat =
      List.fold_right t ~init:(B.pvar h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          Ast_builder.Default.ppat_tuple ~loc [B.pvar tmpvar; acc]
        )
    in
    [%expr
      Bistro.Workflow.app
        (Bistro.Workflow.pure ~id:[%e B.estring id] (fun [%p tuple_pat] -> [%e code]))
        [%e tuple_expr]]
    |> add_renamings deps ~loc

let expression_rewriter ~loc ~path:_ expr =
  let code, deps = new payload_rewriter#expression expr [] in
  build_applicative ~loc deps code

let rec extract_body = function
  | { pexp_desc = Pexp_fun (_,_,_,body) ; _ } -> extract_body body
  | { pexp_desc = Pexp_constraint (expr, ty) ; _ } -> expr, Some ty
  | expr -> expr, None

let rec replace_body new_body = function
  | ({ pexp_desc = Pexp_fun (lab, e1, p, e2) ; _ } as expr) ->
    { expr with pexp_desc = Pexp_fun (lab, e1, p, replace_body new_body e2) }
  | _ -> new_body

let default_descr var =
  Printf.sprintf
    "%s.%s"
    Caml.Filename.(remove_extension (basename !L.input_name))
    var

let str_item_rewriter ~loc ~path:_ descr version var expr =
  let descr = match descr with
    | Some d -> d
    | None -> B.estring (default_descr var)
  in
  let body, body_type = extract_body expr in
  let rewritten_body, deps = new payload_rewriter#expression body [] in
  let applicative_body = build_applicative ~loc deps [%expr fun () -> [%e rewritten_body]] in
  let workflow_body = [%expr
    Bistro.Workflow.cached_value
      ~descr:[%e descr]
      ?version:[%e B.eopt version]
      [%e applicative_body]] in
  let workflow_body_with_type = match body_type with
    | None -> workflow_body
    | Some ty -> [%expr ([%e workflow_body] : [%t ty])]
  in
  [%stri let [%p B.pvar var] = [%e replace_body workflow_body_with_type expr]]


let pstr_item_rewriter ~loc ~path:_ descr version var expr =
  let descr = match descr with
    | Some d -> d
    | None -> B.estring (default_descr var)
  in
  let body, body_type = extract_body expr in
  let rewritten_body, deps = new payload_rewriter#expression body [] in
  let applicative_body = build_applicative ~loc deps [%expr fun __dest__ -> [%e rewritten_body]] in
  let workflow_body = [%expr
    Bistro.Workflow.cached_path
      ~descr:[%e descr]
      ?version:[%e B.eopt version]
      [%e applicative_body]] in
  let workflow_body_with_type = match body_type with
    | None -> workflow_body
    | Some ty -> [%expr ([%e workflow_body] : [%t ty])]
  in
  [%stri let [%p B.pvar var] = [%e replace_body workflow_body_with_type expr]]

let script_rewriter ~loc ~path:_ str =
  let fragments = Script_parser.fragments str in
  List.map fragments ~f:(function
      | `Text (i, j) ->
        let e = B.estring (String.sub str ~pos:i ~len:(j - i + 1)) in
        [%expr Bistro.Shell_dsl.string [%e e]]
      | `Antiquot (i, j) ->
        B.evar (String.sub str ~pos:i ~len:(j - i + 1))
    )
  |> B.elist
  |> (fun e -> [%expr Bistro.Shell_dsl.seq ~sep:"" [%e e]])

let script_ext =
  let open Extension in
  declare "script" Context.expression Ast_pattern.(single_expr_payload (estring __)) script_rewriter
  
let expression_ext =
  let open Extension in
  declare "workflow" Context.expression Ast_pattern.(single_expr_payload __) expression_rewriter

let _np_attr =
  Attribute.declare "bistro.np"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (__))
    (fun x -> x)

let _mem_attr =
  Attribute.declare "bistro.mem"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (__))
    (fun x -> x)

let descr_attr =
  Attribute.declare "bistro.descr"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (__))
    (fun x -> x)

let version_attr =
  Attribute.declare "bistro.version"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (__))
    (fun x -> x)

let str_item_ext label rewriter =
  let open Extension in
  let pattern =
    let open Ast_pattern in
    let vb =
      value_binding ~expr:__ ~pat:(ppat_var __)
      (* |> Attribute.pattern np_attr *)
      (* |> Attribute.pattern mem_attr *)
      |> Attribute.pattern version_attr
      |> Attribute.pattern descr_attr
    in
    pstr ((pstr_value nonrecursive ((vb ^:: nil))) ^:: nil)
  in
    declare label Context.structure_item pattern rewriter

let () =
  Driver.register_transformation "bistro" ~extensions:[
    script_ext ;
    expression_ext ;
    str_item_ext "workflow" str_item_rewriter ;
    str_item_ext "pworkflow" pstr_item_rewriter ;
  ]
