(* TODO: mutual recursion *)
(* TODO: let polymorphism *)
open Syntax 
exception Type_error of string
exception End
type tvar = int 
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TList of ty
  | TTuple of ty list 
  | TVar of tvar
type ty_subst =  (tvar * ty) list
type ty_constraints = (ty * ty) list
type ty_env = (name * ty) list

let new_ty_var  =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in
  body
;;
let rec apply_ty_subst : ty_subst -> ty -> ty =
  fun tysub t ->
  match t with
  | TVar tv -> (try List.assoc tv tysub with
                | Not_found -> TVar tv)
  | TTuple t ->
     TTuple (List.map (fun x -> apply_ty_subst tysub x) t)

  | TList t -> TList (apply_ty_subst tysub t)
  | TFun (tv1, tv2) -> TFun (apply_ty_subst tysub tv1, apply_ty_subst tysub tv2)
  | _-> t
;;
let compose_ty_subst : ty_subst -> ty_subst -> ty_subst =
  fun tysub1 tysub2 ->
  let tysub2' = List.map (fun (tv, t) -> (tv, apply_ty_subst tysub1 t)) tysub2 in
  let tysub1' = List.filter (fun (tv, t) ->  apply_ty_subst tysub2 (TVar tv) = TVar tv ) tysub1 in
  tysub1' @ tysub2'
;;
(* t2の中にt1と同じ型変数があればtrueを返す *)
let rec ty_find :ty -> ty -> bool =
  fun t1 -> function
         | TVar tv when (TVar tv) = t1 -> true
         | TFun (tx, ty) -> ty_find t1 tx || ty_find t1 ty
         | _ -> false
;;
let rec ty_unify : ty_constraints -> ty_subst = function
  | [] -> []
  | (s, t) :: xs when s = t -> ty_unify xs  
  | (TVar tv, t) :: xs | (t, TVar tv) :: xs ->
     let ty_const =
       if ty_find (TVar tv) t then
         raise (Type_error __LOC__)
       else
         List.map (fun (x, y) ->
             (apply_ty_subst [(tv, t)] x, apply_ty_subst [(tv, t)] y)) xs
     in
     compose_ty_subst (ty_unify ty_const) [(tv, t)]
  | (TTuple s, TTuple t) :: xs ->
     let c_li =
      ( try
         List.fold_right2 (fun  x y li-> (x, y)::li) s t []
       with
       | Invalid_argument _ -> raise (Type_error __LOC__))
     in
     compose_ty_subst  (ty_unify c_li) (ty_unify xs)
  | (TList s, TList t) :: xs->
    ty_unify ((s, t) :: xs)
  | (TFun (s, t), TFun (s', t')) :: xs ->
     ty_unify ((s, s') :: (t, t') :: xs) 
  | _ -> raise (Type_error __LOC__)
;;

let rec gather_ty_constraints : ty_env -> expr ->  ty * ty_constraints =
  fun tenv exp ->
  match exp with
  | EValue v ->
     begin
       match v with
       | VInt n -> (TInt, [])
       | VBool b -> (TBool, [])
       | _ -> raise (Type_error __LOC__)
     end
  | EVar name ->
     let t = (try
                List.assoc name tenv
              with
              | Not_found -> raise (Type_error __LOC__)) in
     (t, [])                                
  | EBin (op, e1, e2) ->
     begin
       match op with
       | OpAdd | OpSub | OpMul | OpDiv -> 
          let (t1, c1) = gather_ty_constraints tenv e1 in
          let (t2, c2) = gather_ty_constraints tenv e2 in
          (TInt, (t1, TInt) :: (t1, t2)::c1 @c2)
       | OpEq | OpLt ->
          let (t1, c1) = gather_ty_constraints tenv e1 in
          let (t2, c2) = gather_ty_constraints tenv e2 in
          (TBool, (t1, t2)::c1 @c2)
     end
  | ETuple tuple_li ->
     let t_c_pair =
       List.map (fun x -> gather_ty_constraints tenv x) tuple_li in
     let (t1, c1) =
       List.fold_right (fun  (t, c) (tli, cli) ->
           (t::tli, c @ cli))  t_c_pair ([], []) in
     (TTuple t1, c1)
  | ENil -> (TList (TVar (new_ty_var ())), [])
  | ECons (e1, e2) ->
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let (t2, c2) = gather_ty_constraints tenv e2 in
     (t2, (TList t1, t2) :: c1 @ c2)
  | ELet (p, e1, e2) ->
     let (pt, pe, pc) =
       gather_ty_constraints_pattern p in
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let (t2, c2) = gather_ty_constraints (pe @ tenv) e2 in
     (t2, (pt, t1) :: pc @ c1 @ c2)
  | EIf(e1, e2, e3) ->
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let (t2, c2) = gather_ty_constraints tenv e2 in
     let (t3, c3) = gather_ty_constraints tenv e3 in
     (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
  | EFun (x, e) ->
     let alpha = TVar (new_ty_var ()) in
     let tenv' = (x, alpha) :: tenv in
     let (t, c) = gather_ty_constraints tenv' e in
     (TFun (alpha, t), c)
  | EApp (e1, e2) ->
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let (t2, c2) = gather_ty_constraints tenv e2 in
     let alpha = TVar (new_ty_var ()) in
     (alpha, (t1, TFun (t2, alpha)) :: c1 @ c2)
  | ERLet (f, e1, e2) ->
     let alpha = TVar (new_ty_var ()) in
     let tenv' = (f, alpha) :: tenv in
     let (t1, c1) = gather_ty_constraints tenv' e1 in
     let (t2, c2) = gather_ty_constraints tenv' e2 in
     (t2,  (t1, alpha) :: c1 @ c2)
     (* let alpha  = TVar (new_ty_var ()) in
      * let beta = TVar (new_ty_var ()) in
      * let tenv' = (f, TFun (alpha, beta)) :: tenv in
      * let (t1, c1) = gather_ty_constraints ((x, alpha) :: tenv') e1 in
      * let (t2, c2) = gather_ty_constraints tenv' e2 in
      * (t2, ((t1, beta) :: c1 @ c2)) *)
  | EMatch (e1, pattern_list) ->
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let (pattern_c, ty_of_exp) = 
       List.fold_right (fun (p, e) (cli, etli) ->
           let (pt, pe, pc) =
             gather_ty_constraints_pattern p in
           let (et, ec) =
             gather_ty_constraints (pe @ tenv) e in
           (t1, pt) :: pc @ ec @ cli, et::etli
         ) pattern_list ([], [])
     in
     let et1 = List.hd ty_of_exp in
     let exp_c =
       List.fold_right  (fun x xs -> (et1, x) :: xs)
         (List.tl ty_of_exp) [] in
     (et1, c1 @ exp_c @ pattern_c)

and  gather_ty_constraints_pattern : pattern -> ty * ty_env * ty_constraints =
  fun pattern ->
  match pattern with
  | PInt _ -> (TInt, [], [])
  | PBool _ -> (TBool, [], [])
  | PVar name ->
     let t = TVar (new_ty_var ()) in
     (t, [(name, t)], [])
  | PTuple p_list ->
     let t_tenv_c_list =
       List.map (fun p -> gather_ty_constraints_pattern p) p_list in
     let (t', tenv' , c') =
       List.fold_right (fun (x, y, z) (t, tenv, c) ->
           (x :: t, y  @ tenv, z @ c)) t_tenv_c_list ([], [], [])  in
     (TTuple t', tenv', c')
  | PNil -> (TList (TVar (new_ty_var ())), [], [])
  | PCons (p1, p2) ->
     let (t1, tenv1, c1) = gather_ty_constraints_pattern p1 in
     let (t2, tenv2, c2) = gather_ty_constraints_pattern p2 in
     (t2, tenv1 @ tenv2, (TList t1, t2) :: c1 @ c2)
  | PUnderscore -> (TVar (new_ty_var ()), [], [])
(* match (y,y)  *)

let rec infer_expr : ty_env -> expr -> ty * ty_env =
  fun tenv exp ->
  let (t, c) = gather_ty_constraints tenv exp in
  let tysub' = ty_unify c in
  let t' = apply_ty_subst tysub' t in
  let tenv' = List.map (fun (name, t) -> (name, apply_ty_subst tysub' t)) tenv in
  (t', tenv')
;;    
let rec infer_cmd : ty_env -> command -> ty list  * ty_env =
  fun tenv cmd ->
  match cmd with
  | CExp exp ->
     let (t, env) = infer_expr tenv exp in
     ([t], env)
  | CLet (pattern, e1) ->
     let (pt, pe, pc) = gather_ty_constraints_pattern pattern in
     let (t1, c1) = gather_ty_constraints tenv e1 in
     let tysub' = ty_unify ((pt, t1) :: pc @ c1) in
     let t1' = apply_ty_subst tysub' t1 in
     let env1 = List.map (fun (name, t) -> (name, apply_ty_subst tysub' t)) tenv in
     let pe' = List.map (fun (name, t) -> (name, apply_ty_subst tysub' t) ) pe in
     ([t1'],  pe' @ env1)
  | CRLet (f, exp) ->
     let alpha = TVar (new_ty_var ()) in
     let (t1, c1) = gather_ty_constraints ((f, alpha)::tenv) exp in
     let tysub = ty_unify ((alpha, t1) :: c1) in
     let t' = apply_ty_subst tysub alpha in
     ([t'], (f, t') :: tenv)
     (* let alpha = TVar (new_ty_var ()) in
      * let beta = TVar (new_ty_var ()) in
      * let tenv' = (x, alpha) :: (f, TFun (alpha, beta)) :: tenv in
      * let (t1, c1) = gather_ty_constraints tenv' exp  in
      * let tysub = ty_unify  ((beta, t1):: c1) in
      * let t' = apply_ty_subst tysub (TFun (alpha, beta)) in
      * ([t'], (f,t'):: tenv) *)
  | CMRLet funlist ->
     let tenv' =
       List.fold_right (fun (f, e) env ->
           let alpha = TVar (new_ty_var ()) in
           (f, alpha) :: env
         ) funlist [] in
     let c =
       List.fold_right (fun (f, e) c ->
           let a =
             begin
               try List.assoc f tenv' with
               | Not_found -> raise (Type_error __LOC__)
             end in
           let (t1, c1) = gather_ty_constraints (tenv'@tenv) e in
           (a, t1) :: c1 @ c
         ) funlist [] in 
     let tysub = ty_unify c in
     let tenv_new =
       List.map (fun (x, ty) -> (x, apply_ty_subst tysub ty)) (tenv'@tenv) in
     let tys = List.map (fun (_, ty) -> apply_ty_subst tysub ty) tenv' in
     (tys, tenv_new)
     (* let tenv' =
      *   List.fold_right (fun (f, x, e) env ->
      *       let alpha = TVar (new_ty_var ()) in
      *       let beta = TVar (new_ty_var ()) in
      *       (f, TFun (alpha, beta)) :: env
      *     ) funlist []
      * in
      * let  c  =
      *   List.fold_right  (fun (f, x, e)  c->
      *       let (a, b) =
      *         begin
      *           match  List.assoc f tenv' with
      *           | TFun (alpha, beta) -> (alpha, beta)
      *           | _ -> raise (Type_error __LOC__)
      *         end
      *       in
      *       let (t1, c1) = gather_ty_constraints ((x, a)::tenv'@tenv) e in
      *       (b, t1)::c1 @ c
      *     ) funlist []
      * in
      * let tysub = ty_unify c in
      * let tenv_new = List.map (fun (x, ty) -> (x ,apply_ty_subst tysub ty))  (tenv'@tenv) in
      * let tys = List.map (fun (_, ty) -> apply_ty_subst tysub ty) tenv' in  
      * (tys, tenv_new *)    
  | CEnd -> raise End
;;
