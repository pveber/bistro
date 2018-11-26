module W = Bistro.Internals.Workflow

type error = [
  | `Msg of string
]

let worker f x = f x

let rec eval_main : type s. s W.t -> (s, [> error]) Lwt_result.t = function
  | W.Pure { value ; _ } -> Lwt_result.return value
  | W.App { f ; x ; _ } ->
    let open Lwt_result.Infix in
    let f = eval_main f and x = eval_main x in
    f >>= fun f ->
    x >>= fun x ->
    worker f x |> fun y ->
    Lwt_result.return y
  | W.Both { fst ; snd ; _ } ->
    let open Lwt_result.Infix in
    let fst = eval_main fst and snd = eval_main snd in
    fst >>= fun fst ->
    snd >>= fun snd ->
    Lwt_result.return (fst, snd)
  | W.Eval_path _ -> assert false
  | W.Select _ -> assert false
  | W.Input { path ; _ } -> Lwt_result.return path
  | W.Value { workflow ; _ } ->
    let open Lwt_result in
    eval_main workflow >>= fun build ->
    worker build () |> fun y ->
    Lwt_result.return y
  | W.Path _ -> assert false
  | W.Spawn _ -> assert false

let eval : type s. s Bistro.workflow -> (s, [> error]) Lwt_result.t =
  fun w -> eval_main (W.reveal w)
