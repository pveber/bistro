%{
open Parsetree

let int s =
  { pexp_desc = Pexp_constant (Pconst_integer s) }

let shell_block s =
  { pexp_desc = Pexp_shell_block s }

let value_binding lident exp =
  { pstr_desc = Pstr_value (lident, exp) }
%}

%token EOF
%token <string> INT
%token <string> LIDENT
%token <string> SHELL_BLOCK
%token EQUAL
%token LET

%start program
%type <Parsetree.structure> program

%%

program:
  | list(structure_item) EOF { $1 }
;

structure_item:
  | LET LIDENT EQUAL expression { value_binding $2 $4 }
;

expression:
  | INT { int $1 }
  | SHELL_BLOCK { shell_block $1 }
;
