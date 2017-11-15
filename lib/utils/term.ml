open Core
open Bistro.Std
open Bistro_engine

type 'a path = Path of string

let ( / ) (Path p) (Bistro.Selector q) =
  Path (p ^ (Bistro.Path.to_string q))

type _ t =
  | Pure : 'a -> 'a t
  | PureW : 'a workflow -> 'a path t
  | App : ('a -> 'b) t * 'a t -> 'b t
  | List : 'a t list -> 'a list t

let pure x = Pure x

let pureW w = PureW w

let app f x = App (f, x)

let ( $ ) = app

let list xs = List xs

let assoc xs =
  let keys, terms = List.unzip xs in
  pure (fun values -> List.zip_exn keys values)
  $ list terms

module WHS = Hash_set.Make(
  struct
    open Bistro
    type t = any_workflow
    let hash (Any_workflow u) = String.hash (Workflow.id u)
    let compare (Any_workflow u) (Any_workflow v) =
      String.compare (Workflow.id u) (Workflow.id v)
    let sexp_of_t _ = assert false
    let t_of_sexp _ = assert false
  end
  )

let to_workflow_list term =
  let acc = WHS.create () in
  let rec aux
    : type s. s t -> unit
    = fun t ->
      match t with
      | Pure _ -> ()
      | PureW w ->
        let open Bistro in
        Hash_set.add acc (Any_workflow w)
      | App (f, x) ->
        aux f ; aux x
      | List xs ->
        List.iter xs ~f:aux
  in
  aux term ;
  Hash_set.to_list acc

let rec eval : type s. Db.t -> s t -> s
  = fun db app ->
    match app with
    | Pure x -> x
    | PureW w ->
      Path (Db.workflow_path db w)
    | App (f, x) ->
      (eval db f) (eval db x)
    | List xs ->
      List.map xs ~f:(eval db)


module Syntax = struct
  module Let_syntax = struct
    type nonrec 'a t = 'a t
    let map x ~f = app (pure f) x
    let both x y =
      pure (fun x y -> x, y) $ x $ y
  end
end


let error_short_descr =
  let open Task in
  function
  | Task.Input_check { path ; _ } ->
    sprintf "Input %s doesn't exist" path
  | Select_check { dir_path ; sel ; _ } ->
    sprintf "Path %s doesn't exist in %s" (Bistro.Path.to_string sel) dir_path
  | Step_result { exit_code ; outcome ; _ } ->
    match outcome with
    | `Missing_output -> "Missing output"
    | `Failed ->
      sprintf "Ended with exit code %d" exit_code
    | `Succeeded -> assert false

let error_long_descr db buf tid =
  let open Task in
  function
  | Input_check _
  | Select_check _ -> ()
  | Step_result { action ; dumps ; _ } ->
    (
      match action with
      | `Sh cmd ->
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "| Submitted script                                                             |\n" ;
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "%s\n" cmd
      | `Eval -> ()
    ) ;
    List.iter dumps ~f:(fun (path, text) ->
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "|> Dumped file: %s\n" path ;
        bprintf buf "+------------------------------------------------------------------------------+\n" ;
        bprintf buf "%s\n" text ;
      ) ;
    bprintf buf "#\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "| STDOUT                                                                       |\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "%s\n" (In_channel.read_all (Db.stdout db tid)) ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "| STDERR                                                                       |\n" ;
    bprintf buf "+------------------------------------------------------------------------------+\n" ;
    bprintf buf "%s\n" (In_channel.read_all (Db.stderr db tid))

let error_report_aux db buf = function
  | tid, Scheduler.Run { outcome ; _ } when Task.failure outcome ->
    let short_descr = error_short_descr outcome in
    bprintf buf "################################################################################\n" ;
    bprintf buf "#                                                                              #\n" ;
    bprintf buf "#  Task %s failed\n" tid ;
    bprintf buf "#                                                                               \n" ;
    bprintf buf "#------------------------------------------------------------------------------#\n" ;
    bprintf buf "#                                                                               \n" ;
    bprintf buf "# %s\n" short_descr ;
    bprintf buf "#                                                                              #\n" ;
    bprintf buf "################################################################################\n" ;
    bprintf buf "###\n" ;
    bprintf buf "##\n" ;
    bprintf buf "#\n" ;
    error_long_descr db buf tid outcome
  | _ -> ()

let error_report db traces =
  let buf = Buffer.create 1024 in
  String.Map.to_alist traces
  |> List.iter ~f:(error_report_aux db buf) ;
  Buffer.contents buf


let has_error traces =
  String.Map.exists traces ~f:Scheduler.(function
      | Run { outcome ; _ } -> Task.failure outcome
      | Skipped (`Missing_dep | `Allocation_error _) -> true
      | Skipped `Done_already -> false
    )

let create
    ?(np = 1) ?mem:(`GB mem = `GB 1) ?logger ?(keep_all = true)
    ?(bistro_dir = "_bistro")
    app
  =
  let open Lwt in
  let allocator = Allocator.create ~np ~mem:(mem * 1024) in
  let workflows = to_workflow_list app in
  let dag, goals, precious = Scheduler.compile workflows in
  let config = Task.config ~db_path:bistro_dir ~use_docker:true ~keep_all ~precious in
  Scheduler.(run ?logger ~goals config allocator dag) >>= fun traces ->
  (
    match logger with
    | Some logger ->
      logger#stop ;
      logger#wait4shutdown
    | None -> Lwt.return ()
  ) >|= fun () ->
  if has_error traces then (
    Pervasives.Error (error_report config.Task.db traces)
  )
  else Ok (eval config.Task.db app)

let run ?np ?mem ?logger ?keep_all ?bistro_dir app =
  let thread = create ?np ?mem ?logger ?keep_all ?bistro_dir app in
  match Lwt_main.run thread with
  | Ok x -> x
  | Error msg ->
    prerr_endline msg ;
    failwith "Some workflow failed!"

let dry_run term =
  let workflows = to_workflow_list term in
  let dag, goals, precious = Scheduler.compile workflows in
  let config = Task.config ~db_path:"_bistro" ~use_docker:true ~keep_all:true ~precious in
  Scheduler.dry_run ~goals config dag
  |> Lwt_main.run
