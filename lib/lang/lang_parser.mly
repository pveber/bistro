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
%}

%token EOF
%token <string> INT
%token <string> LIDENT
%token <string> SHELL_WORD
%token SHELL_DEST
%token SHELL_LBRACE
%token RBRACE
%token EQUAL
%token GT
%token SEMICOLON
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
  | LIDENT { ident $1 }
  | SHELL_LBRACE sb = shell_block RBRACE { shell_block sb }
;

shell_block:
  | separated_list(SEMICOLON, shell_cmd) { $1 }
;

shell_cmd:
  | nonempty_list(shell_atom) option(shell_redir) {
          { cmd = $1 ; std_redir = $2 }
        }
shell_atom:
  | SHELL_WORD { Shell_word $1 }
  | SHELL_LBRACE e = expression RBRACE { Shell_antiquot e }
  | SHELL_DEST { Shell_dest }
;

shell_redir:
  | GT nonempty_list(shell_atom) { $2 }
