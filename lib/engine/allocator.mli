type t

type request = Request of {
    np : int ;
    mem : int ;
  }

type resource = Resource of {
    np : int ;
    mem : int ;
  }

val create : np:int -> mem:int -> t
val request : t -> request -> (resource, [`Msg of string]) result Lwt.t
val release : t -> resource -> unit
