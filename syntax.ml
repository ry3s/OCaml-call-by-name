type pattern = PInt  of int
             | PBool  of bool
             | PVar  of name
             | PTuple of pattern list
             | PNil
             | PCons of pattern * pattern
             | PUnderscore 

and expr = EVar   of name 
         | EValue of value
         | EBin   of binOp * expr * expr
         | ETuple of expr list
         | ENil
         | ECons  of expr * expr
         | ELet   of pattern * expr * expr
         | EIf    of expr * expr * expr
         | EFun   of name * expr
         | EApp   of expr * expr
         | ERLet  of name * expr * expr
         | EMatch of expr * (pattern * expr) list 

and value = VInt  of int
          | VBool of bool
          | VTuple of thunk list
          | VNil
          | VCons of thunk * thunk
          | VFun  of name * expr * env
          (* | VRFun of name * name * expr * env *)
          (* | VMRFun of int * (name * expr) list * env *)
          (* | VMRFun of int * thunk list * env *)

and binOp = OpAdd | OpSub | OpMul | OpDiv | OpEq | OpLt
                                                 
and env = (name * thunk) list
        

and name = string

and thunk = Thunk of expr * env
          | RThunk of int * (name * expr) list * env

(* RThunk or lazy_env 
   Thunk of expr * env Lazy.t *)
                   
type command = CExp   of expr
             | CLet   of pattern * expr
             | CRLet  of name * expr
             | CMRLet of (name * expr) list 
             | CEnd
type ('a, 'b) t = Left of 'a | Right of 'b
