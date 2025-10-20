type 'a t =
  | Single_end of 'a
  | Paired_end of 'a * 'a

let map x ~f = match x with
  | Single_end x -> Single_end (f x)
  | Paired_end (x, y) -> Paired_end (f x, f y)

let fst = function
  | Single_end x
  | Paired_end (x, _) -> x
