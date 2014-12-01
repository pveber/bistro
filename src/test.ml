open OUnit

module Config = struct
  let db_path = "_bistro"
  let np = 2
  let mem = 1024
end

module E = Bistro.Engine(Config)

let eval x = Lwt_unix.run (E.eval x)

let add x y =
  let open Bistro.Term in
  Bistro.workflow (
    prim "add" (fun x y _ -> x + y)
    $ int x
    $ int y
  )

let () = assert (eval (add 1 1) = 2)
