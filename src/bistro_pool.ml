let ( >>= ) = Lwt.( >>= )

type t = {
  np : int ;
  mem : int ;
  mutable current_np : int ;
  mutable current_mem : int ;
  mutable waiters : ((int * int) * unit Lwt.u) list ;
}

let create ~np ~mem = {
  np ; mem ;
  current_np = np ;
  current_mem = mem ;
  waiters = [] ;
}

let print p = Lwt_io.printf "{%d %d}\n" p.current_np (List.length p.waiters)

let decr p ~np ~mem =
  p.current_np <- p.current_np - np ;
  p.current_mem <- p.current_mem - mem

let incr p ~np ~mem =
  p.current_np <- p.current_np + np ;
  p.current_mem <- p.current_mem + mem

let acquire p ~np ~mem =
  if np <= p.current_np && mem <= p.current_mem then (
    decr p ~np ~mem ;
    Lwt.return ()
  )
  else (
    let t, u = Lwt.wait () in
    p.waiters <- ((np,mem), u) :: p.waiters ;
    t
  )

let release p ~np ~mem =
  let rec wake_guys_up p = function
    | [] -> []
    | (((np, mem), u) as h) :: t ->
      if np <= p.current_np && mem <= p.current_mem then (
	decr p ~np ~mem ;
	Lwt.wakeup u () ;
	t
      )
      else h :: (wake_guys_up p t)
  in
  incr p ~np ~mem ;
  p.waiters <- wake_guys_up p (List.sort (fun (x, _) (y,_) -> compare y x) p.waiters)

let use p ~np ~mem ~f =
  if np > p.np then Lwt.fail (Invalid_argument "Bistro_pool: asked more processors than there are in the pool")
  else if mem > p.mem then Lwt.fail (Invalid_argument "Bistro_pool: asked more memory than there is in the pool")
  else (
    acquire p ~np ~mem >>= fun () ->
    Lwt.catch
      (fun () ->
	f ~np ~mem >>= fun r -> Lwt.return (`result r))
      (fun exn -> Lwt.return (`error exn))
    >>= fun r ->
    release p ~np ~mem ;
    match r with
    | `result r -> Lwt.return r
    | `error exn -> Lwt.fail exn
  )
