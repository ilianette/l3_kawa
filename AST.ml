open Type


type binop
  = BAdd
  | BMul
  | BDiv
  | BMod
  | BSub

  | BAnd
  | BOr
  | BEq
  | BGT
  | BLT

type expr
  = EUnit
  | EBool of bool
  | EInt of int
  | EVar of ident

  | EBop of expr * binop * expr

  | EBlock of seq 
  | ECond of expr * expr * expr

  | EField of expr * ident
  | ENew of class_name
  | ENewCt of class_name * expr list
  | EMCall of expr * ident * expr list

    
  | EFun of ident option * ident list * expr
  | ECall of expr * expr list

  | ENeg of expr
  | ENot of expr
  | EAdd of expr * expr 
  | EGt of expr * expr
  | EEq of expr * expr
  | EList of expr list

and instr
  = IPrint of expr
  | IAssign of expr * expr
  | IDef of ident * bool * typ option * expr
  | IWhile of expr * seq
  | IRet of expr
  | IExpr of expr
  | ILocRet of expr
and seq = instr list


(* une opération sur les entiers*)
let int_op = function
  | BAdd | BMul | BDiv | BMod | BSub | BGT | BLT -> true
  | _ -> false

let bool_op = function
  | BAnd | BOr -> true
  | _ -> false

module Env = Map.Make(String)



type method_def = {
    (* oui, c'est bizarre d'avoir les deux champs en mutable. En fait, à cause du polymorphisme j'ai besoin d'analyser la méthode avant de donner le type final. Or, dans l'architecture actuel, j'ai pas accès aux fonctions d'analyses depuis le parser.
     *)
    mutable args: (ident * typ) list;
    mutable ret: typ;
    body : seq;
}


type class_def = {
    methods : method_def Env.t;
    fields : typ Env.t;
    elderly : class_name option
}

type program = {
    globals: typ Env.t;
    classes : class_def Env.t;
    main : instr list;
}

let rec fields cenv c = match c.elderly with
  | None -> c.fields
  | Some p -> Env.union (fun _ x _ -> Some x)
                c.fields
                (fields cenv (Env.find p cenv))

let rec meths cenv c = match c.elderly with 
  | None -> c.methods
  | Some p -> Env.union (fun _ x _ -> Some x)
                c.methods
                (meths cenv (Env.find p cenv))



let find_method cl_env cl_name meth_name =
  Env.find meth_name (meths cl_env (Env.find cl_name cl_env))


let find_elderly cl_env cl_name = (Env.find cl_name cl_env).elderly

let find_field_typ cl_env cl_name f_name =
  match Env.find_opt f_name (fields cl_env (Env.find cl_name cl_env)) with
  | Some f -> f
  | None -> let meth = find_method cl_env cl_name f_name in
            tFun (List.map snd meth.args) (meth.ret)


let method_typ cl_env cl_name meth_name =
  let meth = find_method cl_env cl_name meth_name in
  tFun (List.map snd meth.args) meth.ret


      
    
