%token EOF
%token <string> INT
%token <string> LIDENT
%token EQUAL
%token LET

%start program
%type <unit> program

%%

program:
  | list(structure_item) EOF {()}
;

structure_item:
  | LET LIDENT EQUAL INT {()}
;
