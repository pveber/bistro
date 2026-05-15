type 'a t = 'a cmd list

and 'a cmd = {
  cmd : 'a atom list ;
  std_redir : 'a atom option
}

and 'a atom =
  | Word of string
  | Antiquot of 'a
  | Dest
