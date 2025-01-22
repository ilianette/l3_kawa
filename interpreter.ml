open AST

type value
  = VInt of int
  | VBool of bool
  | VObj of obj
  | VFun of closure
  | VList of value list
  | VUnit
  | VNull
and obj = {
    cl_name : string;
    fields : (string, value) Hashtbl.t;
  }
and closure = {
    name: string option;
    env: (string, value) Hashtbl.t;
    args: string list;
    body: expr;
  }

let rec pp_value fmt = function
  | VInt i -> Format.fprintf fmt "VInt(%a)" Format.pp_print_int i
  | VBool b -> Format.fprintf fmt "VInt(%a)" Format.pp_print_bool b
  | VObj _ -> Format.fprintf fmt "obj"
  | VFun _ -> Format.fprintf fmt "closure"
  | VList l -> Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:Type.pp_comma pp_value) l
  | VUnit -> Format.fprintf fmt "()"
  | VNull -> Format.fprintf fmt "NULL"

exception Return of value

let exec_prog (p : program) : unit =
  let cenv = p.classes in
  let env = Hashtbl.create 16 in
  Env.iter (fun k _ -> Hashtbl.add env k VNull) p.globals;

  let [@warning "-8-26"] rec eval_call f args_name args =
    List.iter2 (fun name arg -> Hashtbl.add env name arg)
      args_name
      args
    ;
    
    try exec_seq f; VUnit  with
    | Return(v) ->
       List.iter (fun name -> Hashtbl.remove env name) args_name;
       v

                   

  and [@warning "-8"] eval (e: expr) : value = match e with
    | EInt n -> VInt n
    | EBool b -> VBool b
    | EUnit -> VUnit
    | EVar id -> Hashtbl.find env id


    | ECond(cond,b1,b2) ->
       let v_cond = eval cond in
       begin match v_cond with
       | VBool b -> if b then eval b1 else eval b2
       | _ -> failwith "condition pas un booléen"
       end

    | EField(obj,field) ->
       let v_obj = eval obj in
       begin
         match v_obj with
         | VObj obj -> Hashtbl.find obj.fields field
         | _ -> failwith "pas possible (devrait être objet)"
       end
    | ENew(cl_name) ->
       let fields_name = Env.bindings (Env.find cl_name p.classes).fields |> List.map fst in
       let fields = Hashtbl.create (List.length fields_name) in
       List.iter (fun m_name -> Hashtbl.add fields m_name VNull)
         fields_name;
       
      VObj {cl_name = cl_name; fields = fields}
    | ENewCt(cl_name, args) ->
       call_on_v (eval (ENew cl_name)) "constructor" args

    | EBlock (i::i2::is) -> exec i; eval (EBlock (i2::is))
    | EBlock (i::[]) ->
       begin match i with
       | ILocRet(e) -> eval e
       | _ -> exec i; VUnit
       end
    | EBlock [] -> VUnit

    | ECall(f, args) ->
       let VFun c = eval f in
       Option.iter (fun f -> Hashtbl.add env f (VFun c) ) c.name;
       eval_call [IRet c.body] c.args (List.map eval args)
 
    | EMCall(this, f, args) ->
       let this = eval this in


       let VObj {cl_name=name; fields=_} = this  in

       Hashtbl.add env "this" this;

       let f = find_method cenv name f  in
       let v = eval_call f.body (List.map fst f.args) (List.map eval args) in
       Hashtbl.remove env "this";
       v
             

    | EFun(name, args, body) ->
       VFun { name=name; env=env; args=args; body=body}

    | ENeg e ->
       let (VInt n) = eval e in
       VInt (-n)
    | ENot e ->
       let (VBool b) = eval e in
       VBool (not b)

    | EBop(l, op, r) when int_op op ->
       let (VInt l) = eval l in
       let (VInt r) = eval r in
       begin match op with
       | BAdd -> VInt (l+r)
       | BMul -> VInt (l*r)
       | BDiv -> VInt (l / r)
       | BMod -> VInt (l mod r)
       | BSub -> VInt (l - r)
       | BGT -> VBool (l > r)
       | BLT -> VBool (r < l)
       end
    | EBop(l, op, r) when bool_op op ->
       let (VBool l) = eval l in
       let (VBool r) = eval r in
       begin match op with
       | BAnd -> VBool (l && r)
       | BOr -> VBool (l || r)
       end
    | EBop(l, BEq, r) -> VBool (eval l = eval r)
    | EList(l) -> VList (List.map eval l)

       

  and [@warning "-8"] exec (i: instr) : unit = match i with 
    | IPrint e ->
       begin
         match eval e with
         | VInt n -> Printf.printf "%d\n" n
         | _ -> failwith ""
       end
    | IAssign(e1, e2) -> begin
        match e1 with
        | EVar(id) -> Hashtbl.replace env id (eval e2)
        | EField(obj, field) ->
           let VObj o = eval obj in
           Hashtbl.replace o.fields field (eval e2)
        | _ -> failwith"Bad assign"
      end
    | IDef(id, _, _, expr) ->
       Hashtbl.add env id (eval expr) 
    | IWhile(cond, seq) ->
       let VBool cond = eval cond in
       if cond
       then (exec_seq seq; exec i)

    | IRet e -> raise (Return(eval e))
    | IExpr e | ILocRet e -> eval e |> ignore
  and exec_seq (is: instr list) = List.iter exec is
  and call_on_v (o: value) (m: string) (args: expr list) =
    Hashtbl.add env "temp" o;
    let _ = eval (EMCall(EVar("temp"), m, args)) in
    let v = Hashtbl.find env "temp" in
    Hashtbl.remove env "temp";
    v
  in
  exec_seq p.main

