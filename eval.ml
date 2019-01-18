(* TODO: f p = e -> f x = match x with p -> e *)
(* RThunk of int * (name * expr) list * env *)
open Syntax
open Type   
exception Eval_error of string
exception End

let rec find_match : env -> pattern -> expr -> env option =
  fun env pattern exp ->
  match pattern with
  | PInt pn ->
     if (eval env exp) = VInt pn then Some [] else None
  | PBool pb ->
     if (eval env exp) = VBool pb then Some [] else None
  | PVar px ->
     Some [(px, Thunk (exp, env))]
  | PTuple p_list ->
     (* map2  *)
     let rec loop =
      fun  p_tuple t_list res_env ->
       begin
         match p_tuple, t_list with
         | (px::pxs, (Thunk (e, env0))::txs) ->
            begin
              match find_match env0 px e with
              | Some env' -> loop pxs txs (env'@res_env)
              | None -> None
            end
         | ([], []) -> Some res_env
         | _ -> None
       end
     in
     begin
       match eval env exp with
       | VTuple thunk_list ->  loop p_list thunk_list []
       | _ -> None
     end
  | PNil ->
     begin
       match eval env exp with
       | VNil -> Some []
       | _ -> None
     end
  | PCons (pleft, pright) ->
     begin
       match eval env exp with
       | VCons (Thunk (e1, env1), Thunk (e2, env2)) ->
          let env_left = find_match env1 pleft e1 in
          let env_right = find_match env2 pright e2 in
          begin
            match env_left, env_right with
            | (Some left, Some right) -> Some (left @ right)
            | _ -> None
          end
       | _ -> None
     end
  | PUnderscore -> Some []
                 
and eval : env -> expr -> value = fun env -> function 
  | EValue v -> v
  | EVar x ->
     begin
       match List.assoc x env with
       | Thunk (e, env0) -> eval env0 e
       | RThunk (i, fun_list, env0) ->
          let (f, e) = List.nth fun_list i in
          let env_fun =
            List.mapi (fun i (f, e) ->
                (f, RThunk (i, fun_list, env0))
              ) fun_list
          in
          let env' = env_fun @ env0 in
          eval env' e
     end
  | EBin (op, e1, e2) ->
     let (e1', e2') = (eval env e1, eval env e2) in
     begin
       match (op, e1', e2') with
       | (OpAdd, VInt x1, VInt x2) -> VInt (x1 + x2)
       | (OpSub, VInt x1, VInt x2) -> VInt (x1 - x2)
       | (OpMul, VInt x1, VInt x2) -> VInt (x1 * x2)
       | (OpDiv, VInt x1, VInt x2) -> VInt (x1 / x2)
       | (OpEq , VInt x1, VInt x2) -> VBool (x1 = x2)
       | (OpLt , VInt x1, VInt x2) -> VBool (x1 < x2)
       | (OpEq , VBool b1, VBool b2) -> VBool (b1 = b2)
       | (OpLt , VBool b1, VBool b2) -> VBool (b1 < b2)
       | _ -> raise (Eval_error __LOC__)
     end            
  | ETuple tuple_list -> VTuple (List.map (fun x -> Thunk (x, env)) tuple_list)
  | ENil -> VNil
  | ECons (e1, e2) -> VCons (Thunk (e1, env), Thunk (e2, env))
  | ELet (pattern, e1, e2) ->
     begin
       match find_match env pattern e1 with
       | Some env_new -> eval (env_new@env) e2
       | None -> raise (Eval_error __LOC__)
     end
  | ERLet (f, e1, e2) ->
      let env' =
       (f, RThunk (0,[(f, e1)], env)) :: env in
     eval env' e2
  | EFun (x, e) -> VFun (x, e, env)
  | EApp (e1, e2) ->
     let v1 = eval env e1 in
     begin
       match v1 with 
       | VFun  (x, e, env0) ->
          eval ((x, Thunk (e2, env))::env0) e
       | _ -> raise (Eval_error  __LOC__)
     end
  | EIf (e1, e2, e3) ->
     begin
       match eval env e1 with
       | VBool true -> eval env e2
       | VBool false -> eval env e3
       | _ -> raise (Eval_error __LOC__)
     end
  | EMatch (e1, pattern_list) ->
     let rec check : expr -> (pattern * expr) list -> value =
       fun e p_list ->
       begin
         match p_list with
         | ((pattern, exp)::xs) ->
            let env_option = find_match env pattern e in
            begin
              match env_option with
              | Some [] -> eval env exp
              | Some env_new -> eval (env_new@env) exp
              | None -> check e xs
            end
         | [] -> raise (Eval_error __LOC__)
       end
     in
     check e1 pattern_list 


let execute_cmd : env -> command  -> env * (value, unit) t =
  fun env cmd ->
  match cmd with
  | CExp exp -> (env, Left (eval env exp)) 
  | CLet (pattern, exp) ->
     let env' = 
       begin
         match find_match env pattern exp with
         | Some env_new -> env_new
         | None -> raise (Eval_error __LOC__)
       end
     in
     (env'@env, Right ())
        
  | CRLet (f, e) ->
     ((f, RThunk (0, [(f, e)], env)) :: env, Right ())

  | CMRLet funlist ->
     let env' =
       List.mapi (fun i (f, e) ->
           (f, RThunk (i, funlist, env))
         ) funlist in
    (env' @ env, Right ())
    
  | CEnd -> raise End
