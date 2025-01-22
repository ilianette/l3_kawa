%{
    open AST
    open Type
    let typ_of_ident  = function
      | "int" -> TInt
      | "unit" | "void" -> TUnit
      | "bool" -> TBool
      | c -> tClass c
%}

%token <int> INT
%token <string> IDENT
%token WILDCARD APOSTROPHE
%token TRUE FALSE

%token LPAR RPAR BEGIN END COMMA DOT SEMI
%token PRINT FN VAR VAL ARROW 

%token IF ELSE
%token CLASS METH NEW ATTR EXTENDS
%token ASSIGN WHILE RET

%token EQ GT LT
%token PLUS MINUS TIMES DIV MOD
%token NOT AND OR NEQ 


%token BEGLIST
%token ENDLIST


%token MAIN
%token EOF


%right ARROW

%nonassoc NEQ
%nonassoc EQ GT LT

%left OR
%left AND
%nonassoc NOT

%left PLUS
%left MINUS
%left TIMES
%left DIV
%left MOD

%nonassoc DOT


%start prog
%type <program> prog
%%




prog:
  | globals = gen_seq(glob_decl) classes = list(class_def) MAIN BEGIN s=seq END EOF
    {
      { globals = Env.of_list globals
      ;	classes = Env.of_list classes
      ; main = s
      }
    }	
;

(* stands for general sequence *)
gen_seq(X):
  | { [] }
  | x=X SEMI xs=gen_seq(X) { x::xs }
;

seq:
  | { [] }
  | e=expr { [ILocRet(e)] }
  | i=instr SEMI s=seq  { i::s }
  | i=delimited_instr s=seq { i :: s }
;

delimited_instr:
  | WHILE LPAR cond=expr RPAR BEGIN instrs=seq END { IWhile(cond, instrs) }

typ:
  | APOSTROPHE i=IDENT { QVar (i, []) }
  | WILDCARD { freshvar () }
  | id=IDENT { typ_of_ident id }


  | LPAR RPAR ARROW r=typ { TFun([], r) }
  | arg=typ ARROW ret=typ { TFun([arg], ret) }
  | LPAR args=separated_nonempty_list(COMMA, typ) RPAR ARROW r=typ { TFun(args,r) }
;

block:
  | BEGIN b=seq END { b }
;

expr:
  | LPAR RPAR { EUnit } 
  | n=INT { EInt(n) }
  | TRUE { EBool(true) }
  | FALSE { EBool(false) }
  | BEGIN s=seq END { EBlock s }


  | l=expr PLUS r=expr { EBop(l,BAdd,r) }
  | l=expr MINUS r=expr { EBop(l, BAdd, ENeg r) }
  | MINUS e=expr { ENeg e }

  | l=expr TIMES r=expr { EBop(l, BMul, r) }

  | l=expr DIV r=expr {EBop(l, BDiv, r) }
  | l=expr MOD r=expr { EBop(l, BMod, r) }
  

  | l=expr AND r=expr { EBop(l, BAnd, r) }
  | l=expr OR r=expr { EBop(l, BOr, r) }
  | NOT e=expr { ENot(e) }

  | l=expr GT r=expr { EBop(l,BGT, r) }
  | l=expr LT r=expr { EBop(l,BLT, r) }
  | l=expr GT EQ r=expr { EBop(EBop(l,BGT,r), BOr, EBop(l,BEq,r)) }
  | l=expr LT EQ r=expr { EBop(EBop(l,BLT,r), BOr, EBop(l,BEq,r)) }
  | l=expr EQ r=expr { EBop(l, BEq, r) }
  | l=expr NEQ r=expr { ENot(EBop(l,BEq,r)) }
			    


  | BEGLIST elems=separated_list(COMMA, expr) ENDLIST { EList(elems) }
  | LPAR e=expr RPAR {e}
  | IF LPAR cond=expr RPAR b1=block ELSE b2=block 
    { ECond(cond,EBlock(b1),EBlock(b2)) }


  | NEW x=IDENT { ENew(x) }

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
  | e1=expr ASSIGN e2=expr { IAssign(e1,e2) }

  
  | RET e=expr { IRet(e) } 
  | e=expr { IExpr(e) }
  | v=var_decl { v }
;

%inline var_decl:
  | VAR t=ioption(typ) i=IDENT ASSIGN e=expr
    {
      IDef(i,true,t,e)
    }

  | VAL t=ioption(typ) i=IDENT ASSIGN e=expr
    {
      IDef(i,false,t, e)

    }

%inline glob_decl:
  | VAR t=typ i=IDENT {i, t}



typed_arg:
  | t=typ i=IDENT {i,t}
;
methdef:
  | METH ret=typ name=IDENT LPAR args=separated_list(COMMA, typed_arg)
RPAR BEGIN s=seq END
    {
      let [@warning "-8"] TFun(t_args, t_ret) = Type.generalize (TFun(List.map snd args, ret)) in
      name,
      { ret = t_ret
      ; body = s
      ; args = List.map2 (fun (i,_) t -> i,t) args t_args
      }
    }
;
attrdef:
  | ATTR t=typ name=IDENT { name, t}
;


class_def:				     
  | CLASS name=IDENT parent=option(preceded(EXTENDS, IDENT)) BEGIN
attrs=gen_seq(attrdef) meths=list(methdef) END
    {
      name,
      { fields = Env.of_list attrs
      ; elderly = parent
      ; methods = Env.of_list meths
      }
    }    


