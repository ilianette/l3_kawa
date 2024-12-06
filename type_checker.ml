open AST

module Env = Map.Make(String)

type typ_env = typ Env.t

type typeMissmatch = TypeMissmatch of typ * typ

exception NonHomogeneBlock of typeMissmatch
exception NonHomogeneCond of typeMissmatch
exception NotAnObject of typ
exception GTypeMissmatch of typeMissmatch



let build_tenv prog =
  Env.mapi (fun name _ -> tClass name) prog.classes

let rec typecheck_program prog =
  let tenv = build_tenv prog in
  let cenv = prog.classes in
  let venv = Env.empty in
  
  let rec infer_e venv = function
    | EBool _ -> tBool
    | EInt _ -> tInt
    | EVar name  -> Env.find name tenv

    | EBlock instrs -> infer_block venv instrs
    | ECond(cond, EBlock(b1), EBlock(b2)) -> require venv cond tBool ; 
     let t1 = infer_block venv b1 in
     let t2 = infer_block venv b2 in
     
     if t1 <> t2 then 
       raise (NonHomogeneCond(TypeMissmatch(t1,t2)))
     else
       t1

  | EField(obj, field) ->
     begin
       match infer_e venv obj with
       | St StClass(class_name) -> find_field_typ cenv class_name field  
       | t -> raise (NotAnObject(t))
     end
  | _ -> failwith "bientôt!"

  and infer_block_ venv acc = function
    | [] -> acc
    | (IRet e)::is -> let t = infer_e venv e  in
                      if t <> acc 
                      then raise (NonHomogeneBlock (TypeMissmatch(t,acc)))
                      else infer_block_ venv acc is
                                
    | (IPrint e)::is -> typecheck venv e ; infer_block_ venv acc is
    | IWhile(e,block)::is -> require venv e tBool;
     let t1 = infer_block_ venv acc block in
     let t2 = infer_block_ venv acc is in
     if t1 <> t2
     then raise (NonHomogeneBlock (TypeMissmatch(t1,t2)))
     else acc
     
    | (IAssign(name,e))::is -> require venv e (Env.find name tenv) ;
                               infer_block_ venv acc is
  and infer_block venv  = infer_block_ venv (tUnit) 
  and typecheck venv e = let _ = infer_e venv e in ()
  and require venv e te = let ta = infer_e venv e in
                               if ta <> te
                               then raise (GTypeMissmatch (TypeMissmatch(ta,te)))

  in

  let typecheck_method venv m =
    let venv' = Env.union (fun _ arg var -> Some arg) m.params venv in (* si un argument de la fonction à la même nom qu'une variable, on priorise l'argument.*)
    typecheck venv (EBlock m.body)
  in

  let typecheck_class c =
    (* est incorrect pour une classe dont la.e parent.e a des attributs*)
    let venv' = Env.union (fun _ atr var -> Some atr) c.fields venv in

    Env.iter (fun _ -> typecheck_method venv) c.methods
  in

  Env.iter (fun _ -> typecheck_class) prog.classes
  ;
  typecheck venv (EBlock prog.main)


    
                                                                      

                                                                                                                                     
