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

exception Return of value

let exec_prog (p : program) : unit =
  let cenv = p.classes in
  let env = Hashtbl.create 16 in
  (* je ne sais plus à quoi j'ai pensé que ça servirait
  let env_find = Hashtbl.find env in
  let env_remove = Hashtbl.remove env in
   *)
  

  let [@warning "-8-26"] rec eval_call f args_name args =
    List.iter2 (fun name arg -> Hashtbl.add env name arg)
      args_name
      args
    ;
    
    try exec_seq f; failwith "pas de return" with
    | Return(v) -> List.iter (fun name -> Hashtbl.remove env name) args_name;
                   v
                   

  and [@warning "-8"] eval (e: expr) : value = match e with
    | EInt n -> VInt n
    | EBool b -> VBool b
    | EVar id -> Hashtbl.find env id


    | ECond(cond,b1,b2) ->
       let v_cond = eval cond in
       begin match v_cond with
       | VBool b -> if b then eval b1 else eval b2
       | _ -> failwith ""
       end

    | EField(obj,field) ->
       let v_obj = eval obj in
       begin
         match v_obj with
         | VObj obj -> Hashtbl.find obj.fields field
         | _ -> failwith ""
       end
    | ENew(cl_name) ->
       let fields_name = Env.bindings (Env.find cl_name p.classes).fields |> List.map fst in
       let fields = Hashtbl.create (List.length fields_name) in
       List.iter (fun m_name -> Hashtbl.add fields m_name VNull)
         fields_name;
       
      VObj {cl_name = cl_name; fields = fields}
    | ENewCt(cl_name, args) ->
       eval (EMCall(ENew cl_name, "constructor", args))

    | EBlock (i::i2::is) -> exec i; eval (EBlock (i2::is))
    | EBlock (i::[]) ->
       begin match i with
       | IExpr(e) -> eval e
       | _ -> VUnit
       end
    | EBlock [] -> VUnit

    | ECall(f, args) ->
       let VFun c = eval f in
       Option.iter (fun f -> Hashtbl.add env f (VFun c) ) c.name;
       eval_call [IRet c.body] c.args (List.map eval args)
 
    | EMCall(this, f, args) ->
       let this = eval this in
       let VObj {cl_name=name; fields=_} = this in

       Hashtbl.add env "this" this;

       let f = find_method cenv name f in
               
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
    | EAdd(l,r) ->
       let (VInt l) = eval l in
       let (VInt r) = eval r in
       VInt (l+r)
    | EGt(l,r) ->
       let (VInt l) = eval l in
       let (VInt r) = eval r in
       VBool (l >= r)
    | EEq(l,r) ->
      let (VInt l) = eval l in
      let (VInt r) = eval r in
      VBool (l = r)
    | EList(l) -> VList (List.map eval l)
       

  and [@warning "-8"] exec (i: instr) : unit = match i with 
    | IPrint e ->
       begin
         match eval e with
         | VInt n -> print_int n
         | _ -> failwith ""
       end
    | IAssign(id, expr) ->
       Hashtbl.replace env id (eval expr) 
    | IDef(id, _, _, expr) ->
       Hashtbl.add env id (eval expr) 
    | IWhile(cond, seq) ->
       let VBool cond = eval cond in
       if cond
       then exec_seq seq

    | IRet e -> raise (Return(eval e))
    | IExpr e -> eval e |> ignore
  and exec_seq (is: instr list) = List.iter exec is   
  in
  exec_seq p.main

