open Base

type 'a token =
  | S of string
  | D of 'a
  | F of 'a token list
  | DEST
  | TMP
  | NP
  | MEM

type 'a t = 'a token list

let rec deps tmpl ~compare =
  List.map tmpl ~f:(function
      | D r -> [ r ]
      | F toks -> deps ~compare toks
      | S _ | DEST | TMP | NP | MEM -> []
    )
  |> List.concat
  |> List.dedup_and_sort ~compare

let rec map_token x ~f = match x with
  | S s -> S s
  | D dep -> D (f dep)
  | F toks -> F (List.map toks ~f:(map_token ~f))
  | DEST -> DEST
  | TMP -> TMP
  | NP -> NP
  | MEM -> MEM

let map toks ~f = List.map toks ~f:(map_token ~f)
