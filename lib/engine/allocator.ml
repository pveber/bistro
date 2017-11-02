open Rresult

type request = Request of {
    np : int ;
    mem : int ;
  }

type resource = Resource of {
    np : int ;
    mem : int ;
  }

type t = {
  np : int ;
  mem : int ;
  mutable current_np : int ;
  mutable current_mem : int ;
  mutable waiters : ((int * int) * resource Lwt.u) list ;
}

let create ~np ~mem = {
  np ; mem ;
  current_np = np ;
  current_mem = mem ;
  waiters = [] ;
}

let decr p ~np ~mem =
  p.current_np <- p.current_np - np ;
  p.current_mem <- p.current_mem - mem

let incr p ~np ~mem =
  p.current_np <- p.current_np + np ;
  p.current_mem <- p.current_mem + mem

let request p (Request { np ; mem }) =
  let np = min np p.np in
  if mem > p.mem then
    R.error_msgf
      "Bistro_engine.Allocator: asked more memory than available (%d against %d)"
      mem p.mem
    |> Lwt.return
  else
  if np <= p.current_np && mem <= p.current_mem then (
    decr p ~np ~mem ;
    Lwt.return (Ok (Resource { np ; mem }))
  )
  else (
    let t, u = Lwt.wait () in
    p.waiters <- ((np,mem), u) :: p.waiters ;
    Lwt.(t >|= R.ok)
  )

let release p (Resource { np ; mem }) =
  let rec wake_guys_up p = function
    | [] -> []
    | (((np, mem), u) as h) :: t ->
      if np <= p.current_np && mem <= p.current_mem then (
        decr p ~np ~mem ;
        Lwt.wakeup u (Resource { np ; mem }) ;
        if np = 0 || mem = 0 then t
        else wake_guys_up p t
      )
      else h :: (wake_guys_up p t)
  in
  incr p ~np ~mem ;
  p.waiters <- wake_guys_up p (List.sort (fun (x, _) (y,_) -> compare y x) p.waiters)
