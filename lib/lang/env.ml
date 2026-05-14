module String_map = Map.Make(String)

type value_description = {
  hash : string ;
}

type t = {
  value : value_description String_map.t ;
}

let empty () = {
  value = String_map.empty ;
}

let add_value env lident vd =
  {
    value = String_map.add lident vd env.value ;
  }

let lookup_value_exn env lident =
  String_map.find lident env.value
