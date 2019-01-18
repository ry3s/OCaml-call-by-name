open Syntax
open Eval
open Type
   
exception Print_error of string

let rec print_value : value -> unit = function 
  | VInt i -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VTuple t ->
     let rec loop = function
       | ((Thunk (e, env)) :: []) ->
          print_value (eval env e)
       | ((Thunk (e1, env1)) :: v2)->
           print_value (eval env1 e1);
           print_string ",";
           loop v2
       | _ -> raise (Print_error __LOC__)
     in
     print_string "(";
     loop t;
     print_string ")"
  | VNil -> ()
  | VCons (thunk1, thunk2) ->
     let rec loop = function
       | VCons (Thunk (e1, env1), Thunk (e2, env2)) ->
          let v1 = eval env1 e1 in
          let v2 = eval env2 e2 in
          begin
            match v2 with
            (* | VRFun _ ->
             *    print_value v1;
             *    print_string ";";
             *    print_string "<fun>" *)
            | VNil ->
               print_value v1;
            | _ -> print_value v1;
                   print_string ";";
                   loop v2
          end
       | _ -> raise (Print_error __LOC__)
     in
     print_string "[";
     loop (VCons (thunk1, thunk2));
     print_string "]"
  | VFun (x, e, env) -> print_string ("<fun>")
  (* | VRFun (f, x, e, env) -> print_string ("<fun>")
   * | VMRFun (f, li, env) -> print_string ("<fun>") *)

let rec print_type : ty -> unit = function
  | TInt -> print_string "int"
  | TBool -> print_string "bool"
  | TFun (t1, t2) -> print_type t1;
                     print_string " -> ";
                     print_type  t2
  | TVar tv -> print_string ("t" ^ (string_of_int tv))
  | TTuple t ->
     let rec loop = function
       | (t1 :: []) -> print_type t1
       | (t1 :: t2) -> print_type t1;
                       print_string " * ";
                       loop t2
       | _ -> raise (Print_error __LOC__)
     in
     print_string "(";
     loop t;
     print_string ")"          
  | TList t -> print_type t; print_string " list"
