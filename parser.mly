%{
    open AST
 %}

%token <int> INT
%token <ident> IDENT
%token TRUE FALSE

%token LPAR RPAR BEGIN END COMMA DOT SEMI
%token PRINT

%token IF ELSE
%token CLASS NEW ATTRIBUTE

%token EOF

%start prog
%type <unit> prog
%%

prog:
  | EOF { () }
;

expr:
  | n=INT { EInt(n) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }
  | id=IDENT { EVar(id) }

  | instrs=list(instr) { EBlock instrs }
  | BEGIN instrs=list(instr) END {EBlock instrs }

  | IF cond=expr BEGIN b1=expr END ELSE BEGIN b2=expr END
    { ECond(cond,b1,b2) }

  | NEW x=IDENT { ENew(x) }
  | NEW x=IDENT LPAR arg=separated_list(COMMA, expr) RPAR { ENewCtor(x, arg) }
  | cl=expr DOT meth=IDENT LPAR arg=separated_list(COMMA,expr) RPAR
    { EApp(cl, meth, arg) }
  | cl=expr DOT field=IDENT { EField(cl,field) }
  | name=IDENT { EVar(name) } 
;

instr:
  | dummy=IDENT { IPrint(EBool(false))}
;
