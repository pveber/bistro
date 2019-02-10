open Base

let search ~pat ~annot s =
  let module S = String.Search_pattern in
  let pat = S.create pat in
  S.index_all ~in_:s ~may_overlap:false pat
  |> List.map ~f:(fun x -> x, annot)

let parse_fragments n l =
  let rec wait_opening acc pos = function
    | [] ->
      let acc = if pos < n then `Text (pos, n - 1) :: acc else acc in
      List.rev acc
    | (_, `Close) :: _ -> failwith "Unexpected }}"
    | (i, `Open) :: rest ->
      let acc = if i > pos then `Text (pos, i - 1) :: acc else acc in
      wait_closing acc (i + 2) rest
  and wait_closing acc pos = function
    | [] -> failwith "Unmatched {{, expected }}"
    | (_, `Open) :: _ -> failwith "nested inserts not supported"
    | (j, `Close) :: rest ->
      if pos = j then failwith "empty quotation"
      else
        let acc = `Antiquot (pos, j - 1) :: acc in
        wait_opening acc (j + 2) rest
  in
  wait_opening [] 0 l

let fragments s =
  let opens = search ~pat:"{{" ~annot:`Open s in
  let closes = search ~pat:"}}" ~annot:`Close s in
  parse_fragments (String.length s) (List.sort ~compare:Poly.compare (opens @ closes))
