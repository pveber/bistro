open Core_kernel

type t = string list
[@@deriving sexp]

let compare = List.compare String.compare
let rec normalize = function
  | "." :: t
  | "" :: t -> normalize t
  | h :: ".." :: t ->
    let t = normalize t in
    if h <> ".." then t
    else ".." :: ".." :: t
  | h :: t -> h :: normalize t
  | [] -> []

let common_prefix p q =
  let rec aux res p q =
    match p, q with
    | h_p :: t_p, h_q :: t_q when h_p = h_q ->
      aux (h_p :: res) t_p t_q
    | _ -> List.rev res, p, q
  in
  aux [] p q

let of_string x =
  match String.split ~on:'/' x with
  | "" :: t -> "/" :: t
  | xs -> xs

let make_relative ?from:(q = Sys.getcwd ()) p =
  if Filename.is_relative q
  then invalid_argf "make_rel_path: base %s should be absolute" q ()
  else if Filename.is_relative p then of_string p
  else
    let p = normalize (of_string p)
    and q = normalize (of_string q) in
    let _, p_suffix, q_suffix = common_prefix p q in
    List.map q_suffix ~f:(const "..") @ p_suffix

let rec to_string = function
  | [] -> "."
  | "" :: t -> Filename.concat "." (to_string t)
  | "/" :: t -> "/" ^ (to_string t)
  | p -> List.reduce_exn p ~f:Filename.concat
