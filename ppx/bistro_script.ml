open Base
open Ppxlib

module B = struct
  include Ast_builder.Make(struct let loc = Location.none end)
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
end

module Position = struct
  type t = {
    cnum : int ;
    bol : int ;
    lnum : int ;
  }
  [@@deriving sexp]

  let zero = { cnum = 0 ; bol = 0 ; lnum = 0 }
  let shift p n = { p with cnum = p.cnum + n }
  let newline p =
    { cnum = p.cnum + 1 ;
      bol = p.cnum + 1 ;
      lnum = p.lnum + 1 }

  let translate_lexing_position (q : Lexing.position) ~by:p =
  {
    q with pos_lnum = p.lnum + q.pos_lnum ;
           pos_bol = if p.lnum = 0 then q.pos_bol - 2 else q.pos_cnum + p.bol ;
           pos_cnum = p.cnum + q.pos_cnum
  }

  let move_lexing_pos (p : t) (lp : Lexing.position) =
    { lp with pos_cnum = p.cnum ;
              pos_bol = p.bol ;
              pos_lnum = p.lnum }

  let move_loc (loc : Location.t) p q =
    { loc with loc_start = move_lexing_pos p loc.loc_start ;
               loc_end = move_lexing_pos q loc.loc_end }
end

type token = [
  | `Text of Position.t * Position.t
  | `Antiquotation of Position.t * Position.t
]
[@@deriving sexp]

type lexing_error = [
  | `Unmatched_opening_bracket of Position.t
  | `Unmatched_closing_bracket of Position.t
]
[@@deriving sexp]

type lexing_result = (token list, lexing_error) Result.t
[@@deriving sexp]

let lexer s : lexing_result =
  let n = String.length s in
  let opening i =
    i < n - 1 && Char.(s.[i] = '{' && s.[i + 1] = '{')
  and closing i =
    i < n - 1 && Char.(s.[i] = '}' && s.[i + 1] = '}')
  in
  let classify_current_pos { Position.cnum = i ; _ } =
    if i = n then `EOI
    else if Char.(s.[i] = '\n') then `Newline
    else if opening i then `Opening_bracket
    else if closing i then `Closing_bracket
    else `Text
  in
  let add_text_item acc start stop =
    if Position.(start.cnum < stop.cnum) then `Text (start, stop) :: acc else acc
  in
  let rec loop pos state acc =
    match classify_current_pos pos, state with
    | `EOI, `Quotation p ->
      Ok (List.rev (add_text_item acc p pos))

    | `EOI, `Antiquotation (bracket_pos, _) ->
      Error (`Unmatched_opening_bracket bracket_pos)

    | `Opening_bracket, `Quotation p ->
      let newpos = Position.shift pos 2 in
      loop newpos (`Antiquotation (pos, newpos)) (add_text_item acc p pos)

    | `Opening_bracket, `Antiquotation (bracket_pos, _) ->
      Error (`Unmatched_opening_bracket bracket_pos)

    | `Closing_bracket, `Quotation _ ->
      Error (`Unmatched_closing_bracket pos)

    | `Closing_bracket, `Antiquotation (_, p) ->
      let newpos = Position.shift pos 2 in
      loop newpos (`Quotation newpos) (`Antiquotation (p, pos) :: acc)

    | `Newline, _ ->
      loop (Position.newline pos) state acc

    | `Text, _ ->
      loop (Position.shift pos 1) state acc
  in
  loop Position.zero (`Quotation Position.zero) []

let print_lexing_result r =
  r
  |> sexp_of_lexing_result
  |> Sexp.to_string_hum
  |> Stdio.print_string

let%expect_test "text only" =
  print_lexing_result @@ lexer "rien" ;
  [%expect {| (Ok ((Text (((cnum 0) (bol 0) (lnum 0)) ((cnum 4) (bol 0) (lnum 0)))))) |}]

let%expect_test "text only" =
  print_lexing_result @@ lexer "ad{{a}} {{e}}b" ;
  [%expect {|
    (Ok
     ((Text (((cnum 0) (bol 0) (lnum 0)) ((cnum 2) (bol 0) (lnum 0))))
      (Antiquotation (((cnum 4) (bol 0) (lnum 0)) ((cnum 5) (bol 0) (lnum 0))))
      (Text (((cnum 7) (bol 0) (lnum 0)) ((cnum 8) (bol 0) (lnum 0))))
      (Antiquotation (((cnum 10) (bol 0) (lnum 0)) ((cnum 11) (bol 0) (lnum 0))))
      (Text (((cnum 13) (bol 0) (lnum 0)) ((cnum 14) (bol 0) (lnum 0)))))) |}]

let%expect_test "text + antiquot" =
  print_lexing_result @@ lexer "ri{{en}}{{}}";
  [%expect {|
    (Ok
     ((Text (((cnum 0) (bol 0) (lnum 0)) ((cnum 2) (bol 0) (lnum 0))))
      (Antiquotation (((cnum 4) (bol 0) (lnum 0)) ((cnum 6) (bol 0) (lnum 0))))
      (Antiquotation (((cnum 10) (bol 0) (lnum 0)) ((cnum 10) (bol 0) (lnum 0)))))) |}]

let%expect_test "text + antiquot + eol" =
  print_lexing_result @@ lexer "ri{{en}}\n{{du \n tout}}";
  [%expect {|
    (Ok
     ((Text (((cnum 0) (bol 0) (lnum 0)) ((cnum 2) (bol 0) (lnum 0))))
      (Antiquotation (((cnum 4) (bol 0) (lnum 0)) ((cnum 6) (bol 0) (lnum 0))))
      (Text (((cnum 8) (bol 0) (lnum 0)) ((cnum 9) (bol 9) (lnum 1))))
      (Antiquotation
       (((cnum 11) (bol 9) (lnum 1)) ((cnum 20) (bol 15) (lnum 2)))))) |}]


let translate_position (p : Lexing.position) ~from:(q : Lexing.position) =
  {
    q with pos_lnum = p.pos_lnum + q.pos_lnum - 1 ;
           pos_bol = if p.pos_lnum = 1 then q.pos_bol else q.pos_cnum + p.pos_bol ;
           pos_cnum = p.pos_cnum + q.pos_cnum
  }

class ast_loc_transform = object
  inherit Ast.map
  method bool x = x
  method char c = c
  method int x = x
  method list f x = List.map x ~f
  method option f x = Option.map x ~f
  method string x = x
end

class ast_translation pos = object
  inherit ast_loc_transform
  method! location loc =
    {
      loc with loc_start = translate_position loc.loc_start ~from:pos ;
               loc_end = translate_position loc.loc_end ~from:pos
    }
end

let rewrite str loc =
  let module Location = Ocaml_common.Location in
  match lexer str with
  | Error (`Unmatched_closing_bracket p) ->
    let msg = "Unmatched closing bracket" in
    let loc = Position.move_loc loc p (Position.shift p 2) in
    let err = Location.error ~loc msg in
    raise (Location.Error err)
  | Error (`Unmatched_opening_bracket p) ->
    let msg = "Unmatched opening bracket" in
    let loc = Position.move_loc loc p (Position.shift p 2) in
    let err = Location.error ~loc msg in
    raise (Location.Error err)
  | Ok fragments ->
    List.map fragments ~f:(function
        | `Text (i, j) ->
          let i = i.Position.cnum in
          let j = j.Position.cnum in
          let e = B.estring (String.sub str ~pos:i ~len:(j - i)) in
          [%expr Bistro.Shell_dsl.string [%e e]]
        | `Antiquotation (i, j) ->
          let cnum_i = i.Position.cnum in
          let cnum_j = j.Position.cnum in
          let txt = String.sub str ~pos:cnum_i ~len:(cnum_j - cnum_i) in
          let e = Parser.parse_expression Lexer.token (Lexing.from_string txt) in
          let i' = Position.translate_lexing_position ~by:i loc.loc_start in
          let j' = Position.translate_lexing_position ~by:j loc.loc_start in
          let loc' = Location.{ loc with loc_start = i' ; loc_end = j' } in
          (new ast_translation loc'.loc_start)#expression e
      )
    |> B.elist
    |> (fun e -> [%expr Bistro.Shell_dsl.seq ~sep:"" [%e e]])


let rewriter ~loc:_ ~path:_ { txt = str ; loc } =
  rewrite str loc

let loc_start file_name = {
  Lexing.pos_fname = file_name ;
  pos_lnum = 1 ;
  pos_bol = 0 ;
  pos_cnum = 0 ;
}

let loc_end ~file_name ~file_contents =
  let pos_fname = file_name in
  let last_line_number = Base.String.count file_contents ~f:Char.(( = ) '\n') in
  let last_line_length =
    match Base.String.rindex file_contents '\n' with
    | None -> String.length file_contents
    | Some i -> String.length file_contents - i
  in
  let pos_bol = last_line_length - 1 in
  let pos_cnum = String.length file_contents in
  let pos_lnum = last_line_number in
  { Lexing.pos_fname ; pos_lnum ; pos_bol ; pos_cnum }

let includee_loc ~file_name ~file_contents =
  let loc_start = loc_start file_name in
  let loc_end = loc_end ~file_name ~file_contents in
  { Location.loc_start ; loc_end ; loc_ghost = false }

let include_rewriter ~loc:_ ~path:_ { txt = fn ; loc = _ } =
  match Stdio.In_channel.read_all fn with
  | contents ->
    let loc = includee_loc ~file_name:fn ~file_contents:contents in
    rewrite contents loc
  | exception _ ->
    let msg =
      Printf.sprintf
        "Cannot read %s, for dune users please use preprocessor_deps"
        fn in
    failwith msg
