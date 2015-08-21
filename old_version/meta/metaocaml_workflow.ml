#use "topfind"
#require "core_kernel"

open Core_kernel.Std

type 'a path = Path of string

type _ workflow =
  | Pure : 'a code -> 'a workflow
  | App : ('a -> 'b) workflow * 'a workflow -> 'b workflow
  | Cached : 'a workflow -> 'a workflow
  | Path : (string -> unit) workflow -> string workflow

let digest x =
  let buf = Buffer.create 253 in
  let format = Format.formatter_of_buffer buf in
  Print_code.print_code format x ;
  Digest.(to_hex (string (Buffer.contents buf)))

let rec code_of_worflow : type s. s workflow -> s code = function
  | Pure c -> c
  | App (f, x) ->
    .< .~(code_of_worflow f) .~(code_of_worflow x) >.
  | Cached v ->
    .< .~(code_of_worflow v) >.
  | Path p ->
    let code_p = code_of_worflow p in
    let id = digest code_p in
    .< let () = .~(code_p) id in id  >.

let pure x = Pure x

let ( $ ) f x = App (f, x)

let cached x = Cached x
let path x = Path x

let s_addition = .< fun x y -> x + y >.

let w_addition x y =
  cached (pure s_addition $ x $ y)

let s_int_to_file = .< fun i output ->
    let oc = open_out output in
    Printf.fprintf oc "%d\n" i ;
    Out_channel.close oc
  >.

let w_int_to_file x =
  path (pure s_int_to_file $ x)

let w2 =
  w_int_to_file (w_addition (pure .< 1 >.) (pure .< 1 >.))


let () =
  Print_code.print_code
    Format.std_formatter
    (code_of_worflow w2)
