type 'a workflow = 'a Bistro.workflow

class type ['a,'b] file = object
  method format : 'a
  method encoding : [< `text | `binary] as 'b
end

type 'a directory = [`directory of 'a]
type package = [`package] directory
type 'a zip = ([`zip of 'a], [`binary]) file
type 'a gz = ([`gz of 'a], [`binary]) file constraint 'a = (_,_) #file
type 'a tgz = ([`tgz of 'a],[`binary]) file
type pdf = ([`pdf],[`text]) file
type html = ([`html], [`text]) file
type bash_script = ([`bash_script], [`text]) file

class type ['a] tabular = object ('a)
  constraint 'a = < columns : 'b ; header : ([< `yes | `no] as 'c) ;
    sep : 'd ; comment : 'e ; .. >
  inherit [[`tabular], [`text]] file
  method columns : 'b
  method header : 'c
  method sep : 'd
  method comment : 'e
end

class type ['a] tsv = object
  inherit [ < sep : [`tab] ; .. > as 'a ] tabular
end
