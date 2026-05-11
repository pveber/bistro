%{
open Parsetree

let int s =
  { pexp_desc = Pexp_constant (Pconst_integer s) }

let shell_block sb =
  { pexp_desc = Pexp_shell_block sb }

let value_binding lident exp =
  { pstr_desc = Pstr_value (lident, exp) }
%}

%token EOF
%token <string> INT
%token <string> LIDENT
%token <Parsetree.shell_item> SHELL_ITEM
%token SHELL_LBRACE
%token RBRACE
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
  | SHELL_LBRACE shell_block RBRACE { shell_block $2 }
;

shell_block:
  | list(SHELL_ITEM) { $1 }
;
