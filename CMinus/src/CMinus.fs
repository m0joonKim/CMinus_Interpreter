module CMinus

open AST
open Types
// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var s -> 
    match Map.tryFind s mem with
    | Some i -> i
    | None -> raise UndefinedSemantics
  | Add (e1 , e2) ->
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Int(i1 + i2)
    | _ -> raise UndefinedSemantics
  | Sub (e1 , e2) ->
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Int(i1 - i2)   
    | _ -> raise UndefinedSemantics
  | LessThan (e1, e2) ->
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Bool(i1 < i2)
    | _-> raise UndefinedSemantics
  | GreaterThan (e1, e2) -> 
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Bool(i1 > i2)
    | _-> raise UndefinedSemantics
  | Equal (e1, e2) -> 
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Bool b1, Bool b2) -> Bool(b1=b2)
    | (Int i1, Int i2) -> Bool(i1=i2)
    | _-> raise UndefinedSemantics
  | NotEq (e1, e2) -> 
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Bool b1, Bool b2) -> Bool(b1<>b2)
    | (Int i1, Int i2) -> Bool(i1<>i2)
    | _-> raise UndefinedSemantics  
    
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (str, e) -> Map.add str (evalExp e mem) mem 
  | Seq (s1, s2) -> exec s2 (exec s1 mem)  // TODO: fill in the remaining cases.
  | If (e, t, f) ->
    match evalExp e mem with
    | Bool true -> exec t mem  
    | Bool false -> exec f mem
    | _ -> raise UndefinedSemantics
  | While (e, s) -> 
    match evalExp e mem with
    | Bool true -> 
      let nextMem = exec s mem
      exec (While(e,s)) nextMem
    | Bool false -> mem
    | _ -> raise UndefinedSemantics
  //| _ -> mem
// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty
