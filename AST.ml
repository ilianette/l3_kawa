type ident = string
type class_name = ident

type simple_typ
  = StUnit
  | StInt
  | StBool
  | StClass of class_name

type method_typ = class_name * simple_typ list * typ
and typ
  = St of simple_typ
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



module Env = Map.Make(String)


type method_def = {
    typ : method_typ;
    body :  instr list;
    params : typ Env.t;
}

type m_env = method_def Env.t

type class_def = {
    methods : m_env;
    fields : typ Env.t;
    elderly : class_name option
}


let find_method cl_env cl_name meth_name =
  Env.find meth_name (Env.find cl_name cl_env).methods

let find_field_typ cl_env cl_name f_name =
  match Env.find_opt f_name (Env.find cl_name cl_env).fields with
  | Some f -> f
  | None -> TMeth (find_method cl_env cl_name f_name).typ

