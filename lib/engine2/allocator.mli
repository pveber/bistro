type t
type request = Request of {
    np : int ;
    mem : int ;
  }

type resource = Resource of {
    np : int ;
    mem : int ;
  }

val request : t -> request -> resource Lwt.t
val release : t -> resource -> unit
