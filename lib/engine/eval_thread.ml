open Core_kernel

type 'a t = ('a, Execution_trace.Set.t) Lwt_result.t

module Infix = struct
  let ( >> ) = Lwt.( >>= )
  let ( >>= ) = Lwt_result.( >>= )
  let ( >>| ) = Lwt_result.( >|= )
end

let return = Lwt_result.return
(* let fail = Lwt_result.fail *)
let fail1 e = Lwt_result.fail (Execution_trace.Set.singleton e)
let result_both x y =
  match x, y with
  | Ok x, Ok y -> Ok (x, y)
  | Ok _, Error e -> Error e
  | Error e, Ok _ -> Error e
  | Error e, Error e' -> Error (Execution_trace.Set.union e e')

let both x y =
  Lwt.(x >>= fun x ->
       y >>= fun y ->
       return (result_both x y))

let list_map xs ~f =
  Lwt.bind (Lwt_list.map_p f xs) @@ fun results ->
  let res =
    List.fold results ~init:(Ok []) ~f:(fun acc res ->
        Result.map (result_both acc res) ~f:(fun (xs, x) -> x :: xs)
      )
    |> (
      function
      | Ok xs -> Ok (List.rev xs)
      | Error _ as e -> e
    )
  in
  Lwt.return res

let join2 x y =
  let open Infix in
  both x y >>= fun ((), ()) ->
  return ()

let join xs ~f =
  let open Lwt_result in
  list_map xs ~f >|= ignore

let ignore x = Infix.(x >>| ignore)
