open Base
open! Ppx_core
open Ast_builder.Default

let rec extract_body = function
  | { pexp_desc = Pexp_fun (_,_,_,body) } -> extract_body body
  | expr -> expr

let rec replace_body new_body = function
  | ({ pexp_desc = Pexp_fun (lab, e1, p, e2) } as expr) ->
    { expr with pexp_desc = Pexp_fun (lab, e1, p, replace_body new_body e2) }
  | _ -> new_body

let digest x =
  Caml.Digest.to_hex (Caml.Digest.string (Caml.Marshal.to_string x []))

let new_id =
  let c = ref 0 in
  fun () -> Caml.incr c ; "v" ^ (Int.to_string !c)

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

type payload =
  | Dep of expression
  | Deps of expression

class payload_rewriter = object
  inherit [(string * payload) list] Ast_traverse.fold_map as super
  method! expression expr acc =
    match expr with
    | { pexp_desc = Pexp_extension ({txt = "dep"}, payload) ; pexp_loc = loc } -> (
        match payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _) ; pstr_loc = loc } ] ->
          let id = new_id () in
          let acc' = (id, Dep e) :: acc in
          let expr' = [%expr env#dep [%e B.elident (id ^ "_id")]] in
          expr', acc'
        | _ -> failwith "expected a workflow expression"
      )
    | { pexp_desc = Pexp_extension ({txt = "deps"}, payload) ; pexp_loc = loc } -> (
        match payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _) ; pstr_loc = loc } ] ->
          let id = new_id () in
          let acc' = (id, Deps e) :: acc in
          let expr' = [%expr List.map ~f:env#dep [%e B.elident (id ^ "_id")]] in
          expr', acc'
        | _ -> failwith "expected a workflow list expression"
      )
    | { pexp_desc = Pexp_extension ({txt = ("dest" | "np" | "mem" | "tmp" as ext)}, payload) ; pexp_loc = loc } -> (
        match payload with
        | PStr [] -> (
            let expr' = match ext with
              | "dest" -> [%expr env#dest]
              | "tmp" -> [%expr env#tmp]
              | "np" -> [%expr env#np]
              | "mem" -> [%expr env#mem]
              | _ -> assert false
            in
            expr', acc
          )
        | _ -> failwith "expected empty payload"

      )
    | _ -> super#expression expr acc

end

let rewriter ~loc ~path descr version mem np var expr =
  let open Ast_builder.Default in
  let np = Option.value np ~default:(B.eint 1) in
  let mem = Option.value mem ~default:(B.eint 100) in
  let descr = Option.value descr ~default:(B.estring var) in
  let version = Option.value version ~default:(B.eint 0) in
  let body = extract_body expr in
  let rewriter = new payload_rewriter in
  let code, deps = rewriter#expression body [] in
  let id = digest code in
  let add_bindings body = List.fold deps ~init:body ~f:(fun acc (tmpvar, payload) ->
      match payload with
      | Dep expr ->
        [%expr
          let [%p B.pvar tmpvar] = [%e expr] in
          let [%p B.pvar (tmpvar ^ "_id")] =
            Bistro.Workflow.to_dep [%e B.elident tmpvar] in
          [%e acc]]
      | Deps expr ->
        [%expr
          let [%p B.pvar tmpvar] = [%e expr] in
          let [%p B.pvar (tmpvar ^ "_id")] =
            List.map ~f:Bistro.Workflow.to_dep [%e B.elident tmpvar] in
          [%e acc]]
    )
  in
  let dep_list =
    List.map deps ~f:(fun (v, payload) ->
        match payload with
        | Dep _ -> [%expr [ Bistro.Workflow.u [%e B.elident v] ]]
        | Deps _ -> [%expr List.map ~f:Bistro.Workflow.u [%e B.elident v]]
      )
    |> B.elist
  in
  let new_body = [%expr
    let id = [%e B.estring id] in
    let f env : unit = [%e code] in
    Bistro.Workflow.of_fun
      ~descr:[%e descr]
      ~np:[%e np]
      ~mem:[%e mem]
      ~version:[%e version]
      ~id ~deps:(List.concat [%e dep_list]) f
  ] |> add_bindings
  in
  [%stri let [%p B.pvar var] = [%e replace_body new_body expr]]

let np_attr =
  Attribute.declare "bistro.np"
    Attribute.Context.value_binding
    Ast_pattern.(single_expr_payload (__))
    (fun x -> x)

let mem_attr =
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

let ext =
  let open Ppx_core in
  let open Extension in
  let pattern =
    let open Ast_pattern in
    let vb =
      value_binding ~expr:__ ~pat:(ppat_var __)
      |> Attribute.pattern np_attr
      |> Attribute.pattern mem_attr
      |> Attribute.pattern version_attr
      |> Attribute.pattern descr_attr
    in
    pstr ((pstr_value nonrecursive ((vb ^:: nil))) ^:: nil)
  in
  declare "bistro" Context.structure_item pattern rewriter

let () =
  Ppx_driver.register_transformation "bistro" ~extensions:[ ext ]
