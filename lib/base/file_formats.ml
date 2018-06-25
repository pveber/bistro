(** Conventional type to represent file targets. The object type is to
    represent properties of the file, like the type of encoding (text
    or binary) or the format. *)

class type file = object
  method file_type : [`regular]
end

(** Conventional type to represent directory targets *)
class type ['a] directory = object
  method file_type : [`directory]
  method contents : 'a
end

class type text_file = object
  inherit file
  method encoding : [`text]
end

class type binary_file = object
  inherit file
  method encoding : [`binary]
end

(** Conventional type to represent OCaml values saved with the
    {!module:Marshal} module. *)
class type ['a] value = object
  inherit binary_file
  method format : [`marshalled_value]
  method content_type : 'a
end


(** Conventional type to represent OCaml values saved as
    S-expressions. *)
class type ['a] sexp_value = object
  inherit text_file
  method format : [`sexp_value]
  method content_type : 'a
end

class type pdf = object
  inherit text_file
  method format : [`pdf]
end

class type html = object
  inherit text_file
  method format : [`html]
end

class type png = object
  inherit binary_file
  method format : [`png]
end

class type svg = object
  inherit text_file
  method format : [`svg]
end

class type tsv = object
  inherit text_file
  method colum_separator : [`tab]
end
