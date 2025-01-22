open Type
open AST

module Env = Map.Make(String)
type typ_env = typ Env.t


exception UnificationImpossible of typ * typ
exception NotAssignable of expr
exception Unknown of ident
exception BadArgs of typ list * typ list

let rec origin cenv = function
  | TClass name -> begin
      match (Env.find name cenv).elderly with
      | Some p -> origin cenv (tClass p)
      | None -> (tClass name)
    end
  | t -> t


(* Vrai si t1 <: t2.*)
let rec coerceable env t1 t2 =
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
   On retourne aussi l'unificateur et on fait quelques bidouilles par commodité  *)
let [@warning "-57"] rec unify t1 t2 = if t1 == t2 then t1 else 
  match t1, t2 with
  | TVar {contents = Bound t1}, t2 | t2, TVar {contents = Bound t1}
    -> unify t1 t2

  | TVar ({contents = Unbound _} as t1), t2 | t2, TVar ({ contents = Unbound _ } as t1) -> 
    occurs t1 t2;
    t1 := Bound t2 ; t2

  
  | TFun([], r1) , TFun([tUnit], r2) | TFun([tUnit], r2) , TFun([],r1)
    -> TFun([tUnit], unify r1 r2)
  
  | TFun(args1,ret1), TFun(args2, ret2) when List.length args1 = List.length args2 
    -> TFun (List.map2 unify args1 args2, unify ret1 ret2)

  | TClass x, TClass y when x = y -> TClass x                          
  | (TClass x, d  | d, TClass x) when d = dummy_class  -> TClass x

  | _ -> UnificationImpossible(t1, t2) |> raise




(*
non-commutative !
si t1 <: t2 donne t1 sinon donne l'unificateur de t1 et t2
*)
let coerceable_or_unify cenv t1 t2 =
  if coerceable cenv t1 t2 then t1 else unify t1 t2


let typecheck_program prog =
  let cenv = prog.classes in
  let venv = prog.globals in

  let fields = fields cenv in
  let origin = origin cenv in
  let coerceable_or_unify = coerceable_or_unify cenv in

  let find i venv = try Env.find i venv with | _ -> Unknown i |> raise in 
  
  let [@warning "-8-26"] rec infer venv = function
    | EBool _ -> TBool
    | EInt _ -> TInt
    | EUnit -> TUnit

    | EVar name  ->  degeneralize (find name venv)

    | ENeg e -> unify TInt (infer venv e)

    | ENot e -> unify TBool (infer venv e)

    | EBop(e1,b,e2) -> begin
        let t = unify (infer venv e1) (infer venv e2) in
        match b with
        | BAdd | BMul | BDiv | BMod | BSub -> unify TInt t
        | BAnd | BOr -> unify TBool t
        | BGT | BLT -> unify TInt t |> ignore; TBool
        | BEq -> TBool
      end

    | EList l ->
       let t = List.fold_left unify (freshvar ()) (List.map (infer venv) l) in
       TList t

    | EBlock instrs -> infer_block venv instrs

    (* Le contenu des ifs est toujours un block, cf le parser. *)
    | ECond(cond, EBlock(b1), EBlock(b2)) ->
       unify (infer venv cond) TBool |> ignore;
       
       let t1 = infer_if_b venv b1 in
       let t2 = infer_if_b venv b2 in
       unify (origin t1) (origin t2) 

    | EField(obj, field) ->
       let TClass cl_name = unify (infer venv obj) (tClass "") in
       degeneralize (find_field_typ cenv cl_name field)

    | ENew cl_name -> tClass cl_name
    | ENewCt(cl_name, args) ->
       let t_args = List.map (infer venv) args in
       let t_ct_args = List.map snd (find_method cenv cl_name "constructor").args   in

       List.map2 unify t_args t_ct_args |> ignore;
       tClass cl_name


    | EMCall(caller, meth_name, args) ->
       let t_caller = infer venv caller in
       let t_args = List.map (infer venv) args in
       
       let TClass cl_name = unify t_caller (tClass "") in 

       let TFun(t_m_args, t_ret) = degeneralize (method_typ cenv cl_name meth_name) in     

       if (List.length t_args <> List.length t_m_args) then BadArgs(t_m_args, t_args) |> raise;
       List.map2 unify t_args t_m_args|> ignore;
       
       t_ret
    | ECall(f, args) ->
       let t_args = List.map (infer venv) args in
       
       let TFun(t_f_args, t_ret) = unify (infer venv f) (tFun t_args (freshvar ())) in
       (try List.map2 coerceable_or_unify t_args t_f_args |> ignore;
           t_ret
       with Not_found -> BadArgs(t_f_args, t_args) |> raise)

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

  and typecheck_i venv = function
    | IPrint e -> unify TInt (infer venv e) |> ignore; venv

    | IAssign((EVar _) as e1,e2) | IAssign((EField _) as e1, e2)
      -> coerceable_or_unify (infer venv e2) (infer venv e1) |> ignore; venv
    | IAssign(e,_) -> NotAssignable(e) |> raise

    | IDef(var, _, typ, init) ->
       let t_init = (infer venv init) in
       Option.map (fun t -> unify t_init (degeneralize t)) typ |> ignore;
       Env.add var (generalize t_init) venv

    | IWhile(cond, seq) ->
       unify (infer venv cond) TBool |> ignore;
       typecheck_seq venv seq

    | IRet e | ILocRet e | IExpr e -> infer venv e |> ignore; venv
  and  typecheck_seq venv seq = List.fold_left typecheck_i venv seq



  and infer_block venv is =
    let rec loop venv is acc = match is with
      | [] -> acc
      | (IRet e)::is ->
         let t = infer venv e  in
         begin match acc with
         | None -> origin t |> Option.some
         | Some acc -> loop venv is
                         (Some (unify acc (origin t)))
               
         end

      (* ensuite on fait que propager et typecheck au passage. *)
      | IWhile(_, instrs) as i :: is -> let venv = typecheck_i venv i in
                                        let acc_while = loop venv instrs acc in
                                        loop venv is acc_while 
      | IExpr(EBlock instrs as e) :: is ->
         infer venv e |> ignore;
         loop venv is (loop venv instrs acc)

      (* le contenu des ifs est toujours un block. Cf le parser *)
      | IExpr(ECond(_,EBlock(b1),EBlock(b2)) as e)::is ->
         infer venv e |> ignore;
         let acc1 = loop venv b1 acc in
         let acc2 = loop venv b2 acc1 in
         loop venv is acc2

      |  i::is -> loop (typecheck_i venv i) is acc
    in
    
    Option.value ~default:TUnit (loop venv is None) 


  (* c'est en soit un fold avec un cas d'arêt un peu spécial *)
  and infer_if_b venv = function
    | [] -> TUnit
    | [ILocRet e] -> infer venv e
    | i::is -> infer_if_b (typecheck_i venv i) is 
  in 

  
  let [@warning "-8"] typecheck_method  venv name m =
    let venv = Env.union (fun _ arg _ -> Some arg) (Env.of_list m.args) venv in
    let f = EFun(Some name, List.map fst m.args, EBlock m.body) in

    let TFun(args, ret) = generalize (infer venv f) in
    
    m.ret <- ret;
    m.args <- List.map2 (fun (name, _) t -> name, t) m.args args;

  in

  let typecheck_class name c =
    let venv =
      Env.union (fun _ atr _ -> Some atr) (fields c) venv
      |> Env.add "this" (tClass name) in

    Env.iter 
      (typecheck_method venv)
      c.methods
  in
   
  Env.iter typecheck_class prog.classes;
  infer venv (EBlock prog.main) |> ignore
   
  
