open Rresult
open Bistro_engine


type 'a result = ('a, R.msg) Rresult.result

type cfg = {
  host : string ;
  port : int
}

val post_wave : cfg -> string -> Db.Wave.t -> unit result Lwt.t
