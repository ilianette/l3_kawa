open Type
open AST


module Env = Map.Make(String)

type typ_env = typ Env.t

type typeMissmatch = TypeMissmatch of typ * typ

exception NonHomogeneBlock of typeMissmatch
exception NonHomogeneCond of typeMissmatch
exception NotAnObject of typ
exception GTypeMissmatch of typeMissmatch
exception ArgsTypeMissmatch
exception MethDoesNotExist 
exception UnificationImpossible of typ * typ


let rec origin cenv = function
  |TClass name -> begin
      match (Env.find name cenv).elderly with
      | Some p -> origin cenv (tClass p)
      | None -> (tClass name)
    end
  | t -> t


(* Vrai si t1 <: t2.*)
let rec coerceable env t1 (* to *) t2 =
  if t1 = t2 then true else
    match t1, t2 with
    | TClass x, TClass y -> begin
        match (Env.find x env).elderly with
        | None -> false
        | Some p -> p = y || coerceable env (tClass p) t2
      end
    | QVar (_,constraints) , t2 ->
       List.exists (fun t1 -> coerceable env t1 t2) constraints          
    | _ -> false

(* crash si la première variable apparaît dans la seconde.*)
let rec occurs tv = function 
  | TVar ({contents = Bound t} as tv2) ->
    if tv = tv2 then 
      failwith "type cyclique"
    else 
      occurs tv t 
  | _ -> ()


(* fais les substitutions nécessaires pour que t1 et t2 deviennent leur unificateur principal.
   On retourne aussi l'unificateur par commodité.
   On fait aussi deux trois bidouilles par commodité (pour les classes et les fonctions "sans" arguments
 *)
let rec unify t1 t2 = if t1 == t2 then t1 else 
  match t1, t2 with
  | TVar {contents = Bound t1}, t2 | t2, TVar {contents = Bound t1}
    -> unify t1 t2
  | TVar ({contents = Unbound _} as t1), t2 | t2, TVar ({ contents = Unbound _ } as t1) -> 
    occurs t1 t2;
    t1 := Bound t2 ; t2

  
  | TFun([], r1) , TFun([tUnit], r2) | TFun([tUnit], r2) , TFun([],r1)
    -> TFun([tUnit], unify r1 r2)
  
  | TFun(args1,ret1), TFun(args2, ret2) ->
     if List.length args1 = List.length args2
     then TFun (List.map2 unify args1 args2, unify ret1 ret2)
     else UnificationImpossible(t1,t2) |> raise

  | TClass x, TClass y when x = y -> TClass x
                                          
  | TClass x, d  | d, TClass x  -> if d = dummy_class then TClass x else UnificationImpossible(t1,t2) |> raise
  | _ -> UnificationImpossible(t1, t2) |> raise

let rec generalize = function 
  | TVar { contents = Unbound x } -> QVar(x, [])
  | TFun(args, ret) -> TFun(List.map generalize args, generalize ret)
  | TList t -> TList (generalize t)
  | t -> t


(* donnera un jour une variable fresh, c'est pas dur à écrire *)


(*
non-commutative !
si t1 <: t2 donne t1 sinon donne l'unificateur de t1 et t2
*)
let coerceable_or_unify cenv t1 t2 =
  if coerceable cenv t1 t2 then t1 else unify t1 t2


let typecheck_program prog e =
  let cenv = prog.classes in
  let venv = Env.empty in

  let origin = origin cenv in
  let coerceable_or_unify = coerceable_or_unify cenv in
  
  
  let [@warning "-8-26"] rec infer venv = function
    | EBool _ -> TBool
    | EInt _ -> TInt
    | EVar name  -> Env.find name venv

    | ENeg e -> unify TInt (infer venv e)

    | ENot e -> unify TBool (infer venv e)

    | EAdd(e1, e2) -> unify TInt (infer venv e1)
                    |> ignore; unify TInt (infer venv e2)

    | EEq(e1, e2) -> unify (infer venv e1) (infer venv e2)
                   |> ignore; TBool
    | EGt(e1, e2) -> unify TInt (infer venv e1)
                   |> ignore; unify TInt (infer venv e2)
                   |> ignore; TBool

    | EList l ->
       List.fold_left unify (freshvar ()) (List.map (infer venv) l)

    | EBlock instrs -> infer_block venv instrs

    | ECond(cond, EBlock(b1), EBlock(b2)) ->
       unify (infer venv cond) TBool |> ignore;
       
       let t1 = infer_if_b venv b1 in
       let t2 = infer_if_b venv b2 in
       unify (origin t1) (origin t2) 


    | EField(obj, field) ->
       let TClass cl_name = unify (infer venv obj) (tClass "") in
       find_field_typ cenv cl_name field

    | ENew cl_name -> tClass cl_name

    | ENewCt(cl_name, args) ->
       let t_args = List.map (infer venv) args in
       let t_ct_args = List.map snd (find_method cenv cl_name "constructor").args in
       
       List.map2 unify t_args t_ct_args |> ignore;

       tClass cl_name


    | EMCall(caller, meth_name, args) ->
       let t_caller = infer venv caller in
       (* une classe sans nom s'unifie à n'importe quelle classe, ça n'a pas vraiment de sens si ce n'est pour faciliter l'écriture ici.*)
       let TClass cl_name = unify t_caller (tClass "") in 

       let meth = find_method cenv cl_name meth_name in
       List.map2 unify (List.map (infer venv) args) (List.map snd meth.args) |> ignore;
       meth.ret 

    | ECall(f, args) ->
       let t_args = List.map (infer venv) args in
       
       let TFun(t_f_args, t_ret) = unify (infer venv f) (tFun t_args (freshvar ())) in

       List.map2 coerceable_or_unify t_args t_f_args |> ignore;
       t_ret

    | EFun(name, args, body) ->

       let t_args = List.map (fun _ -> freshvar ()) args  in
       
       let fun_env =
         List.combine args t_args
         |> Env.of_list 
         |> Env.union (fun _ _ arg ->  Some arg) venv
       in

       let t_ret = match name with
         | None -> infer fun_env body 
         | Some f -> infer (Env.add f (tFun t_args (freshvar ())) fun_env) body
       in
       tFun t_args t_ret
    |  _-> failwith "pas encore implémenté!"

  and typecheck_i venv = function
    | IPrint e -> unify TInt (infer venv e) |> ignore; venv

    | IAssign(var,e) -> coerceable_or_unify  (infer venv e) (infer venv (EVar var)) |> ignore; venv

    | IDef(var, _, typ, init) ->
       let t_init = infer venv init in
       
       Option.map (unify t_init) typ |> ignore;
       Env.add var t_init venv

    (*
      | IMethDef(class_name, name, args, ret, body) ->
      let env =
      (Env.find cl_name cenv).fields
      |> Env.union (fun _ var _ -> var) venv 
      |> Env.add "this" (tClass cl_name)
      in
      let (St (TFun(args, ret))) =
      infer env (EFun(Some name, tClass cl_name:: List.map fst args, body))
      in
      TMeth {
      self: cl_name;
      coerceable_or_unify (tClass cl_name) (List.hd args);
     *)
    | IWhile(cond, seq) ->
       unify (infer venv cond) TBool |> ignore;
       typecheck_seq venv seq

    | IRet e | IExpr e -> infer venv e |> ignore; venv
  and  typecheck_seq venv seq = List.fold_left typecheck_i venv seq




  and infer_block_aux venv is acc = match is with
    | [] -> acc
    | (IRet e)::is ->
       let t = infer venv e  in
       begin match acc with
       | None -> origin t |> Option.some
       | Some acc -> infer_block_aux venv is
                       (Some (unify acc (origin t)))
                   
       end
    | IWhile(_, instrs) as i :: is -> let venv = typecheck_i venv i in
                                      infer_block_aux venv is (infer_block_aux venv instrs acc)
    | IExpr(EBlock instrs as e) :: is ->
       infer venv e |> ignore;
       infer_block_aux venv is (infer_block_aux venv instrs acc)

    | IExpr(ECond(_,b1,b2) as e)::is ->
       infer venv e |> ignore;
       let acc = infer_block_aux venv ([IExpr(b1)]) acc in
       infer_block_aux venv is (infer_block_aux venv ([IExpr(b2)]) acc)

    |  i::is -> let venv = typecheck_i venv i in infer_block_aux venv is acc   
 
  and infer_block venv is = Option.value ~default:TUnit (infer_block_aux venv is None) 

  (* c'est en soit un fold avec un cas d'arêt un peu spécial *)
  and infer_if_b venv = function
    | [] -> TUnit
    | [IExpr e] -> infer venv e
    | i::is -> infer_if_b (typecheck_i venv i) is 
  in 

  (*
  let typecheck_method venv m =
    let venv = Env.union (fun _ arg _ -> Some arg) (Env.of_list m.args) venv in (* si un argument de la fonction a le même nom qu'une variable, on priorise l'argument.*)
    typecheck venv (EBlock m.body)
  in

  
  let typecheck_class c =
    (* est incorrect pour une classe dont la.e parent.e a des attributs*)
    let venv = Env.union (fun _ atr _ -> Some atr) c.fields venv in

    Env.iter (fun _ -> typecheck_method venv) c.methods
  in
   *)
 (* let typecheck_e venv e t =
    let t_e = infer venv e in
    (coerceable cenv t_e t) ||
      try unify (infer venv e) t |> ignore; true
      with _ -> false
  in
  *)
  generalize (infer venv e)
  (*
  
  Env.iter (fun _ -> typecheck_class) prog.classes;
  typecheck venv (EBlock prog.main);
   *)
  
