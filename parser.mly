%{
    open AST
    open Type
    let typ_of_ident _ = TUnit
%}

%token <int> INT
%token <string> IDENT
%token WILDCARD
%token TRUE FALSE

%token LPAR RPAR BEGIN END COMMA DOT SEMI
%token PRINT FN VAR VAL ARROW

%token IF ELSE
%token CLASS METH NEW ATTR EXTENDS
%token ASSIGN WHILE RET

%token EQ GT
%token PLUS 
%token MINUS

%token BEGLIST
%token ENDLIST

%token LBRACKET
%token RBRACKET

%token MAIN
%token EOF


%nonassoc ARROW

%nonassoc EQ GT
%left PLUS
%nonassoc MINUS
%nonassoc DOT

%start prog
%type <program> prog
%%

prog:
  | classes = list(class_def) MAIN BEGIN s=seq END
    {
      { classes = Env.of_list classes
      ; main = s
      }
    }	
;


typ:
  | id=IDENT { typ_of_ident id }
  | LPAR RPAR { TUnit }

  | LPAR RPAR ARROW r=typ { TFun([], r) }
  | LPAR args=separated_nonempty_list(COMMA, typ) RPAR ARROW r=typ { TFun(args,r) }
;


expr:
  | n=INT { EInt(n) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }
  | BEGIN s=seq END {EBlock s }


  | l=expr PLUS r=expr { EAdd(l,r) }
  | MINUS e=expr { ENeg e }
  | l=expr GT r=expr { EGt(l,r) }
  | l=expr EQ r=expr { EEq(l,r) }


  | BEGLIST elems=separated_list(COMMA, expr) ENDLIST { EList(elems) }
  | LPAR e=expr RPAR {e}
  | IF cond=expr BEGIN b1=expr END ELSE BEGIN b2=expr END
    { ECond(cond,b1,b2) }


  | NEW x=IDENT LPAR RPAR { ENew(x) }

  | NEW x=IDENT LPAR arg=separated_nonempty_list(COMMA, expr) RPAR { ENewCt(x, arg) }


  | obj=expr DOT meth=IDENT LPAR arg=separated_list(COMMA,expr) RPAR
    { EMCall(obj, meth, arg) }
  | obj=expr DOT field=IDENT { EField(obj,field) }


  | name=IDENT { EVar(name) }

  | LPAR f=fn RPAR { f }

  | e=fun_call { e }


;

fun_call:
  | LPAR f=fn RPAR LPAR args=separated_list(COMMA, expr) RPAR { ECall(f, args) }
  | f=IDENT LPAR args=separated_list(COMMA, expr) RPAR { ECall(EVar(f), args) }

fn:
  | FN name=option(IDENT) LPAR RPAR ARROW e=expr { EFun(name, [], e) }
  | FN name=option(IDENT) LPAR args=separated_nonempty_list(COMMA, IDENT) RPAR ARROW  e=expr {EFun(name,args,e)}

instr:
  | PRINT LPAR e=expr RPAR { IPrint(e) }
  | x=IDENT ASSIGN e=expr { IAssign(x,e) }
  | WHILE LPAR cond=expr RPAR BEGIN instrs=seq END { IWhile(cond, instrs) }
  | RET e=expr { IRet(e) }
  | e=expr { IExpr(e) }

  | VAR t=ioption(typ) i=IDENT ASSIGN e=expr
    {
      IDef(i,true,t,e)
    }

  | VAL t=ioption(typ) i=IDENT ASSIGN e=expr
    {
      IDef(i,false,t, e)

    }

;
%inline seq:
| instrs=separated_list(SEMI, instr) { instrs }
;




methdef:
  | METH ret=typ name=IDENT LPAR args=separated_list(COMMA, pair(IDENT, typ))
RPAR BEGIN s=seq END
    { name, {ret=ret; body=s; args=args; self=""} }
;
attrdef:
  | ATTR t=typ name=IDENT { name, t }
;

class_def:
  | CLASS name=IDENT parent=option(preceded(EXTENDS, IDENT)) BEGIN
attrs=separated_list(SEMI, attrdef) meths=list(methdef)
    {
      name,
      { fields = Env.of_list attrs
      ; elderly = parent
      ; methods = Env.of_list meths
      }
    }    

