open Core.Std
open Bistro.Std
open Bistro_engine

type _ t =
  | Pure : 'a -> 'a t
  | PureW : _ workflow * (string -> 'a) -> 'a t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | List : 'a t list -> 'a list t

let pure x = Pure x

let pureW w f = PureW (w, f)

let app f x = App (f, x)

let ( $ ) = app

let list xs = List xs

let rec to_workflow_list
  : type s. s t -> Bistro.any_workflow list
  = function
    | Pure _ -> []
    | PureW (w, _) -> [ Bistro.Workflow w ]
    | App (f, x) ->
      to_workflow_list f @ to_workflow_list x
    | List xs ->
      List.map xs ~f:to_workflow_list
      |> List.concat

let rec eval : type s. Db.t -> s t -> s
  = fun db app ->
    match app with
    | Pure x -> x
    | PureW (w, f) -> f (Db.workflow_path db w)
    | App (f, x) ->
      (eval db f) (eval db x)
    | List xs ->
      List.map xs ~f:(eval db)


let rec string_of_path = function
  | [] -> "."
  | "" :: t -> Filename.concat "." (string_of_path t)
  | p -> List.reduce_exn p ~f:Filename.concat

let error_report db xs =
  List.iter xs ~f:(fun (dep, msg) ->
      match dep with
      | `Input i ->
        fprintf stderr "################################################################################\n" ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "#  Invalid input %s\n" (string_of_path i) ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "# %s\n" msg ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "################################################################################\n"

      | `Select (tid, p) ->
        fprintf stderr "################################################################################\n" ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "#  Invalid select: no %s in %s\n" (string_of_path p) tid ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "# %s\n" msg ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "################################################################################\n" ;

      | `Task tid ->
        let report = match Db.Task_table.get db tid with
          | Some t -> Db.report db t
          | None -> sprintf "Unregistered task %s" tid
        in
        fprintf stderr "################################################################################\n" ;
        fprintf stderr "#                                                                              #\n" ;
        fprintf stderr "#  Task %s failed\n" tid ;
        fprintf stderr "#                                                                               \n" ;
        fprintf stderr "#------------------------------------------------------------------------------#\n" ;
        fprintf stderr "#                                                                               \n" ;
        fprintf stderr "# %s\n" msg ;
        fprintf stderr "#                                                                              #\n" ;
        prerr_endline report
    )

let collect_errors results =
  List.map results ~f:(function
      | Ok _ -> []
      | Error xs -> xs
    )
  |> List.concat
  |> List.dedup

let run ?use_docker ?(np = 1) ?(mem = 1024) ?tmpdir app =
  let open Lwt in
  let backend = Scheduler.local_backend ?use_docker ?tmpdir ~np ~mem () in
  let main =
    let db = Db.init_exn "_bistro" in
    let scheduler = Scheduler.make backend db in
    let workflows = to_workflow_list app in
    Scheduler.build_all scheduler workflows >|=
    collect_errors >>= (
      function
      | [] -> return (eval db app)
      | errors ->
        error_report db errors ;
        fail (Failure "Bistro_app failed!")
    )
  in
  Lwt_unix.run main


type repo_item =
  Repo_item : string list * _ workflow -> repo_item

let ( %> ) path w = Repo_item (path, w)

let rec string_of_path = function
  | [] -> "."
  | "" :: t -> Filename.concat "." (string_of_path t)
  | p -> List.reduce_exn p ~f:Filename.concat

let make_absolute p =
  if Filename.is_absolute p then p
  else Filename.concat (Sys.getcwd ()) p

let link p p_u =
  let dst = string_of_path p in
  let src = make_absolute p_u in
  Unix.mkdir_p (Filename.dirname dst) ;
  let cmd = sprintf "rm -rf %s && ln -s %s %s" dst src dst in
  ignore (Sys.command cmd)

let generate_page outdir (dest, cache_path) =
  link (outdir :: dest) cache_path

let of_repo ~outdir items =
  List.map items ~f:(function Repo_item (p, w) ->
      pure (generate_page outdir) $ pureW w (fun s -> p, s)
    )
  |> list
  |> app (pure ignore)
