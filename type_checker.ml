open AST

module Env = Map.Make(String)

type typ_env = typ Env.t

type typeMissmatch = TypeMissmatch of typ * typ 
exception NonHomogeneBlock of typeMissmatch
exception NonHomogeneCond of typeMissmatch
exception NotAnObject of typ
exception GTypeMissmatch of typeMissmatch


let rec infer_e cenv tenv = function
  | EBool _ -> Ts TBool
  | EInt _ -> Ts TInt
  | EVar name  -> Env.find name tenv

  | EBlock instrs -> infer_block cenv tenv instrs
  | ECond(cond, EBlock(b1), EBlock(b2)) -> require cenv tenv cond (Ts TBool) ; 
     let t1 = infer_block cenv tenv b1 in
     let t2 = infer_block cenv tenv b2 in
     
     if t1 <> t2 then 
       raise (NonHomogeneCond(TypeMissmatch(t1,t2)))
     else
       t1
  | EField(obj, field) ->
     begin
       match infer_e cenv tenv obj with
       | Ts TClass(class_name) -> find_field cenv class_name field  
       | t -> raise (NotAnObject(t))
     end
  | _ -> failwith "bientôt!"

and infer_block_ cenv tenv acc = function
  | [] -> acc
  | (IRet e)::is -> let t = infer_e cenv tenv e  in
                    if t <> acc 
                    then raise (NonHomogeneBlock (TypeMissmatch(t,acc)))
                    else infer_block_ cenv tenv acc is
                                
  | (IPrint e)::is -> type_check cenv tenv e ; infer_block_ cenv tenv acc is
  | IWhile(e,block)::is -> require cenv tenv e (Ts TBool);
     let t1 = infer_block_ cenv tenv acc block in
     let t2 = infer_block_ cenv tenv acc is in
     if t1 <> t2
     then raise (NonHomogeneBlock (TypeMissmatch(t1,t2)))
     else acc
     
  | (IAssign(name,e))::is -> require cenv tenv e (Env.find name tenv) ; infer_block_ cenv tenv acc is
and infer_block cenv tenv = infer_block_ cenv tenv (Ts TUnit) 
and type_check cenv tenv e = let _ = infer_e cenv tenv e in ()
and require cenv tenv e te = let ta = infer_e cenv tenv e in
                             if ta <> te
                               then raise (GTypeMissmatch (TypeMissmatch(ta,te)))
 
