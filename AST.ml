open Type

type expr
  = EBool of bool
  | EInt of int
  | EVar of ident

  | EBlock of instr list 
  | ECond of expr * expr * expr

  | EField of expr * ident
  | ENew of class_name
  | ENewCt of class_name * expr list
  | EMCall of expr * ident * expr list
  | ECall of expr * expr list
    
  | EFun of ident option * ident list * expr

  | ENeg of expr
  | ENot of expr
  | EAdd of expr * expr 
  | EGt of expr * expr
  | EEq of expr * expr
  | EList of expr list
and instr
  = IPrint of expr
  | IAssign of ident * expr
  | IDef of ident * bool * typ option * expr
  | IWhile of expr * instr list
  | IRet of expr
  | IExpr of expr



 
let freshvar () = TVar (ref (Unbound ""))

module Env = Map.Make(String)


(* faire un peu de ménage là dedans *)
type method_def = {
    (*typ : method_typ;*)
    body :  instr list;
    args: (ident * typ) list;
    self : class_name;
    ret: typ
}

type m_env = method_def Env.t

type class_def = {
    methods : m_env;
    fields : typ Env.t;
    elderly : class_name option
}

type program = {
    classes : class_def Env.t;
    main : instr list;
}


let find_method cl_env cl_name meth_name =
  Env.find meth_name (Env.find cl_name cl_env).methods

let find_elderly cl_env cl_name = (Env.find cl_name cl_env).elderly

let find_field_typ cl_env cl_name f_name =
  match Env.find_opt f_name (Env.find cl_name cl_env).fields with
  | Some f -> f
  | None -> let meth = find_method cl_env cl_name f_name in
            tFun (List.map snd meth.args) (meth.ret)
                                                          

