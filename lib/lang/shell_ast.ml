type 'a t = 'a cmd list

and 'a cmd = {
  cmd : 'a atom list ;
  std_redir : 'a atom option
}

and 'a atom =
  | Word of string
  | Antiquot of 'a
  | Dest

let map xs ~f =
  let map_atom = function
    | Word s -> Word s
    | Antiquot x -> Antiquot (f x)
    | Dest -> Dest
  in
  let map_cmd c = {
    cmd = List.map map_atom c.cmd ;
    std_redir = Option.map map_atom c.std_redir ;
  }
  in
  List.map map_cmd xs
