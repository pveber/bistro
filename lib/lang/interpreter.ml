type path =
  | FS of string
  | Cache of string
[@@deriving show]

type value =
  | VInt of int
  | VPath of path
[@@deriving show]

module String_map = Map.Make(String)

module Env = struct
  type t = value Lwt.t String_map.t
  let empty = String_map.empty
  let add (e : t) k v = String_map.add k v e
  let get_exn (e : t) k = String_map.find k e
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

let lwt_option_map o ~f =
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
  let%lwt stdout = lwt_option_map stdout ~f:redirection in
  Lwt_process.exec ?stdout ("", cmd)

type t = {
  db : string ;
}

let create dir =
  Db.create dir ;
  { db = dir }

let eval_const : Typedtree.constant -> value = function
  | Tconst_int i -> VInt i

let rec eval_expression itp env (exp : Typedtree.expression) =
  match exp.texp_desc with
  | Texp_constant k -> Lwt.return (eval_const k)
  | Texp_ident lident -> Env.get_exn env lident
  | Texp_shell_block sb ->
    exec_shell_block itp env ~hash:exp.texp_hash sb

and exec_shell_block itp env ~hash cmds =
  let%lwt cmds = Lwt_list.map_p (eval_shell_cmd itp env ~hash) cmds in
  let rec loop = function
    | [] -> Lwt.return (VPath (Cache hash))
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
  let eval_atom : Typedtree.expression Shell_ast.atom -> string Lwt.t = function
    | Word s -> Lwt.return s
    | Antiquot e ->
        let%lwt v = eval_expression itp env e in
        let s = match v with
          | VInt i -> string_of_int i
          | VPath (FS p) -> p
          | VPath (Cache id) -> Db.cache_path itp.db id
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

and eval_structure itp str_items =
  let env = Env.empty in
  let str_items, env = List.fold_left (eval_str_item itp env) ([], env) str_items in
  Lwt_list.map_p Fun.id (List.rev str_items)

and eval_str_item itp env (acc, env) item =
  match item.Typedtree.tstr_desc with
  | Typedtree.Tstr_value (lident, exp) ->
    let value = eval_expression itp env exp in
    let env = Env.add env lident value in
    value :: acc, env
