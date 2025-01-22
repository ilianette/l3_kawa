type ident = string
type class_name = ident


type typ
  = TVar of tvar ref
  | QVar of ident * constraints (* n'est pas encore utilisé *)

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
let tVar name = TVar (ref (Unbound name))

let dummy_class = TClass ""


let counter_maker =
  let compteur = ref 0 in 
  fun () -> 
    compteur := !compteur + 1 
    ;
      !compteur
let counter () = counter_maker ()

let freshvar = fun () ->
  let n = counter () in
  TVar (ref (Unbound ((
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
  else "t" ^ string_of_int n))))




(* clôture d'une formule, lie les variables libres avec des pour-tout *)
let rec generalize =  function 
  | TVar { contents = Unbound x } -> QVar(x, [])
  | TVar { contents = Bound t } -> generalize t
  | TFun (args, ret) -> TFun(List.map generalize args, generalize ret)
  | TList t -> TList (generalize t)
  | t -> t

(* la réciproque de 'generalize'. Elle se débarasse des quantificateurs du type tout en préservant le sens en renommant.*)
let degeneralize t =
  let rec loop subst t = match t with
    | QVar(name,_) -> begin
        try (List.assoc name subst, subst)
        with Not_found ->
              let var = freshvar () in
              (var, (name,var)::subst)
      end
    | TVar {contents = Bound t} -> loop subst t
    | TList t ->
       let (t, subst) = loop subst t in
       TList t, subst
    | TFun (args, ret) ->
       (* c'est un peu subtil ici. On ne peut pas maper notre fonction sur les arugments, on pourrait sinon transformer la même variable liée en plusieurs variables libres différentres.*)
       let (args, subst) =
         List.fold_right
           (fun t (types, subst) ->
             let (nt, subst) = loop subst t in
             (nt::types, subst))
           args ([], subst)
       in
       
       let (ret, subst) = loop subst ret in
       TFun (args, ret), subst
    | t -> t, subst
  in
 fst (loop [] t)


let pp_comma fmt () = Format.fprintf fmt ","

let rec pp_typ fmt = function 
  | TVar {contents = Bound t} -> pp_typ fmt t
  | TVar {contents = Unbound v } -> Format.fprintf fmt "%a" Format.pp_print_string v
  | TUnit -> Format.fprintf fmt "unit"
  | TBool -> Format.fprintf fmt "bool"
  | TInt -> Format.fprintf fmt "int"
  | TString -> Format.fprintf fmt "string"
  | TList t -> Format.fprintf fmt "list<%a>" pp_typ t
  | TFun (args,ret) -> Format.fprintf fmt "(%a) -> %a" (Format.pp_print_list ~pp_sep:pp_comma pp_typ) args pp_typ ret
  | TClass x -> Format.fprintf fmt "class(%a)" Format.pp_print_string x
  | QVar (x, _) -> Format.fprintf fmt "Q(%a)" Format.pp_print_string x


