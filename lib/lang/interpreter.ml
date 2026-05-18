module String_map = Map.Make(String)

module Env = struct
  type t = Lambda.expression Lwt.t String_map.t

  let empty = String_map.empty

  let register (e : t) k value =
    String_map.add k value e

  let lookup_exn (e : t) k = String_map.find k e
end

module Db = struct
  let cache_directory db = Filename.concat db "cache"
  let build_directory db = Filename.concat db "build"

  let create path =
    let perm = 0o700 in
    Unix.mkdir path perm ;
    Unix.mkdir (build_directory path) perm ;
    Unix.mkdir (cache_directory path) perm

  let cache_path db id = Filename.concat (cache_directory db) id
  let build_path db id = Filename.concat (build_directory db) id
  let build_dest db id = Filename.concat (build_path db id) "dest"
end

let lwt_option_bind o ~f =
  match o with
  | None -> Lwt.return None
  | Some x ->
    let%lwt y = f x in
    Lwt.return (Some y)

let exec_exn cmd =
  match%lwt Lwt_process.exec ("", cmd) with
  | WEXITED 0 -> Lwt.return ()
  | _ -> Lwt.fail_with (String.concat " " @@ Array.to_list cmd)

let mv src dst =
  exec_exn [| "mv" ; src ; dst |]

let redirection filename =
  let flags = Unix.([O_APPEND ; O_CREAT ; O_WRONLY]) in
  let%lwt fd = Lwt_unix.openfile filename flags 0o640 in
  Lwt.return (`FD_move (Lwt_unix.unix_file_descr fd))

let lwt_exec ?stdout cmd =
  let%lwt stdout = lwt_option_bind stdout ~f:redirection in
  Lwt_process.exec ?stdout ("", cmd)

type t = {
  db : string ;
}

let create dir =
  Db.create dir ;
  { db = dir }

let lwt_shell_ast_bind xs ~f =
  let open Shell_ast in
  let bind_atom = function
    | Word s -> Lwt.return (Word s)
    | Antiquot x ->
      let%lwt y = f x in
      Lwt.return (Antiquot y)
    | Dest -> Lwt.return Dest
  in
  let map_cmd c =
    let%lwt cmd = Lwt_list.map_p bind_atom c.cmd
    and std_redir = lwt_option_bind c.std_redir ~f:bind_atom in
    Lwt.return { cmd ; std_redir }
  in
  Lwt_list.map_p map_cmd xs

let rec eval_expression itp env (exp : Lambda.expression) =
  match exp.Lambda.desc with
  | Lconst _ -> Lwt.return exp
  | Lvar lident -> Env.lookup_exn env lident
  | Lshell sb ->
    exec_shell_block itp env ~hash:exp.hash sb
  | Llam _ -> Lwt.return exp

and exec_shell_block itp env ~hash cmds =
  let%lwt cmds = Lwt_list.map_p (eval_shell_cmd itp env ~hash) cmds in
  let rec loop = function
    | [] -> Lwt.return Lambda.(Exp.path (Cache hash))
    | h :: t ->
      match%lwt exec_shell_cmd itp h with
      | Ok () -> loop t
      | Error e -> Lwt.fail e
  in
  Unix.mkdir (Db.build_path itp.db hash) 0o700 ;
  let%lwt path = loop cmds in
  let%lwt () = mv (Db.build_dest itp.db hash) (Db.cache_path itp.db hash) in
  Lwt.return path

and exec_shell_cmd itp (cmd, redir) =
  let%lwt status = lwt_exec cmd ?stdout:redir in
  let failed = match status with
    | WEXITED 0 -> false
    | WEXITED _
    | WSIGNALED _
    | WSTOPPED _ -> true
  in
  Lwt.return (
    if failed then Error (Failure "shell failed")
    else Ok ()
  )

and eval_shell_cmd itp env { Shell_ast.cmd ; std_redir } ~hash =
  let eval_atom : Lambda.expression Shell_ast.atom -> string Lwt.t = function
    | Word s -> Lwt.return s
    | Antiquot e ->
        let%lwt v = eval_expression itp env e in
        let s = match v.desc with
          | Lconst c -> (
              match c with
              | Constant_int i -> string_of_int i
              | Constant_path (FS p) -> p
              | Constant_path (Cache id) -> Db.cache_path itp.db id
            )
          | _ -> assert false
        in
        Lwt.return s
    | Dest -> Lwt.return (Db.build_dest itp.db hash)
  in
  let%lwt cmd = Lwt_list.map_p eval_atom cmd
  and std_redir = match std_redir with
    | None -> Lwt.return None
    | Some atom ->
      let%lwt s = eval_atom atom in
      Lwt.return (Some s)
  in
  Lwt.return (Array.of_list cmd, std_redir)

and eval_program itp defs =
  let env = Env.empty in
  let str_items, env = List.fold_left (eval_def itp env) ([], env) defs in
  Lwt_list.map_p Fun.id (List.rev str_items)

and eval_def itp env (acc, env) (lident, exp) =
  let value = eval_expression itp env exp in
  let env = Env.register env lident value in
  value :: acc, env
