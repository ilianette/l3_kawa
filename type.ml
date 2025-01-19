type ident = string
type class_name = ident


type typ
  = TVar of tvar ref
  | QVar of ident * constraints (* variable de type quantifié (un pour tout, le plus à gauche possible) qui vérifie les contraintes de sous-typage *)

  | TUnit
  | TBool
  | TInt
  | TString
  | TClass of class_name

  | TFun of typ list * typ
  | TList of typ
and tvar
  = Unbound of ident
  | Bound of typ
and constraints
  = typ list


let tClass name = TClass name
let tFun args ret = TFun(args, ret) 
 
let dummy_class = TClass ""

