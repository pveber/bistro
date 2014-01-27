open Core.Std

type path = string list
type item = Item of Bistro_workflow.u * string * path
type t = Repo of item list

let item ?(descr = "") path workflow = Item ((workflow : 'a Bistro_workflow.t :> Bistro_workflow.u), descr, path)

let find_duplicate_paths repo =
  List.(
    map repo ~f:(function Item (_,_,x) -> x)
    |! find_a_dup
  )

let fail_on_duplicate_paths items = match find_duplicate_paths items with
| None -> ()
| Some p ->
    let path = String.concat ~sep:"/" p in
    failwithf "Path %s is present several times in the repo!" path ()

let make items =
  fail_on_duplicate_paths items ;
  Repo items

let setup ?(wipeout = false) db (Repo items) repo_base =
  if Sys.file_exists repo_base <> `Yes then Sys.command_exn (sprintf "mkdir -p %s" repo_base) ;
  if wipeout then Sys.command_exn (sprintf "rm -rf %s/*" repo_base) ;
  List.iter items ~f:(
    function Item (workflow,_,rel_path)  ->
      let path = repo_base ^ "/" ^ (String.concat ~sep:"/" rel_path) in
      let cache_path = Bistro_db.path db workflow in

      (* Create link if needed *)
      let create_link =
	if Sys.file_exists path = `Yes then Unix.(
	  if (lstat path).st_kind <> S_LNK || readlink path <> cache_path
	  then (
	    unlink path ;
	    true
	  )
	  else false
	)
	else true
      in
      if create_link then (
	Sys.command_exn (sprintf "mkdir -p %s" (Filename.dirname path)) ;
	Sys.command_exn (sprintf "ln -s `readlink -f %s` %s" cache_path path) 
      )
  )
