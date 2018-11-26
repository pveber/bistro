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

let rewriter ~loc ~path:_ expr =
  let rewriter = new payload_rewriter in
  let code, deps = rewriter#expression expr [] in
  let id = digest (string_of_expression code) in
  let add_renamings init =
    List.fold deps ~init ~f:(fun acc (tmpvar, expr, ext) ->
        let rhs = match ext with
          | Path  -> [%expr Bistro.eval_path [%e expr]]
          | Param -> [%expr Bistro.pure_data [%e expr]]
          | Value -> expr
        in
        [%expr let [%p B.pvar tmpvar] = [%e rhs] in [%e acc]]
      )
  in
  match deps with
  | [] ->
    [%expr Bistro.pure ~id:[%e B.estring id] [%e code]]
  | (h_tmpvar, _, _) :: t ->
    let tuple_expr =
      List.fold_right t ~init:(B.elident h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          [%expr Bistro.both [%e B.elident tmpvar] [%e acc]]
        )
    in
    let tuple_pat =
      List.fold_right t ~init:(B.pvar h_tmpvar) ~f:(fun (tmpvar,_,_) acc ->
          Ast_builder.Default.ppat_tuple ~loc [B.pvar tmpvar; acc]
        )
    in
    [%expr
      Bistro.app
        (Bistro.pure ~id:[%e B.estring id] (fun [%p tuple_pat] -> [%e code]))
        [%e tuple_expr]]
    |> add_renamings

let ext =
  let open Extension in
  declare "bistro" Context.expression Ast_pattern.(single_expr_payload __) rewriter

let () =
  Driver.register_transformation "bistro" ~extensions:[ ext ]
