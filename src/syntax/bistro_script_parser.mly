%{
  open Printf

  let typ_of_string = function
    | "i" -> `int
    | "s" -> `string
    | "f" -> `float
    | "w" -> `workflow
    | "bin" -> `pkg_bin
    | "PATH" -> `PATH
    | x -> failwith (sprintf "Unknown conversion id %s" x)
%}

%token <string> STRING
%token <string * Camlp4.PreCast.Syntax.Ast.expr> ANTIQUOT
%token EOL EOF DEST TMP
%token <string> SPACE
%token <char> QUOTE

%start script
%type <Bistro_script_ast.t> script

%%

script:
| commands EOF
    { Bistro_script_ast.simplify (List.rev $1) }
;

commands:
|
    { [] }
| commands EOL
    { $1 }
| commands EOL command
    { (List.rev $3) :: $1 }
;

command:
| token
    { [ $1 ] }
| command token
    { $2 :: $1 }
;

token:
| STRING
    { Bistro_script_ast.S $1 }
| ANTIQUOT
    { let (k,expr) = $1 in
      Bistro_script_ast.ANTIQUOT (typ_of_string k, expr) }
| DEST
    { Bistro_script_ast.D }
| TMP
    { Bistro_script_ast.TMP }
| SPACE
    { Bistro_script_ast.SPACE $1 }
| QUOTE
    { Bistro_script_ast.QUOTE $1 }
;
