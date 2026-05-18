%{
open Parsetree

let int s =
  { pexp_desc = Pexp_constant (Pconst_integer s) }

let ident s =
  { pexp_desc = Pexp_ident s }

let shell_block sb =
  { pexp_desc = Pexp_shell_block sb }

let value_binding lident exp =
  { pstr_desc = Pstr_value (lident, exp) }

let structure ?input defs = { pmod_inputs = input ; pmod_defs = defs }
%}

%token EOF
%token <string> INT
%token <string> LIDENT
%token <string> SHELL_WORD
%token SHELL_DEST
%token SHELL_LBRACE
%token LBRACE
%token RBRACE
%token EQUAL
%token GT
%token SEMICOLON
%token LET
%token INPUT_DIRECTIVE

%start program
%type <Parsetree.structure> program

%%

program:
  | option(input_section) list(structure_item) EOF { structure ?input:$1 $2 }
;

input_section:
  | INPUT_DIRECTIVE LBRACE list(structure_item) RBRACE { $3 }
;

structure_item:
  | LET LIDENT EQUAL expression { value_binding $2 $4 }
;

expression:
  | INT { int $1 }
  | LIDENT { ident $1 }
  | SHELL_LBRACE sb = shell_block RBRACE { shell_block sb }
;

shell_block:
  | separated_list(SEMICOLON, shell_cmd) { $1 }
;

shell_cmd:
  | nonempty_list(shell_atom) option(shell_redir) {
          { Shell_ast.cmd = $1 ; std_redir = $2 }
        }
shell_atom:
  | SHELL_WORD { Shell_ast.Word $1 }
  | SHELL_LBRACE e = expression RBRACE { Shell_ast.Antiquot e }
  | SHELL_DEST { Shell_ast.Dest }
;

shell_redir:
  | GT shell_atom { $2 }
