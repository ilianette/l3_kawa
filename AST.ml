type ident = string
type class_name = ident

type simple_typ
  = TUnit
  | TInt
  | TBool
  | TClass of class_name

type method_typ = class_name * simple_typ list * typ
and typ
  = Ts of simple_typ
  | TMeth of method_typ


type expr
  = EBool of bool
  | EInt of int
  | EVar of ident

  | EBlock of instr list 
  | ECond of expr * expr * expr

  | EField of expr * ident
  | ENew of class_name
  | ENewCt of class_name * expr list
  | EApp of expr * ident * expr list
and instr
  = IPrint of expr
  | IAssign of ident * expr
  | IWhile of expr * instr list
  | IRet of expr
   
type typed_expr = { expr: expr; typ: typ }





type method_def = {
    name : ident;
    typ : method_typ;
    body :  instr list;
}

type class_def = {
    name : class_name;
    methods : method_def list;
    attributes : (ident * simple_typ) list;
    elderly : class_name option
}


