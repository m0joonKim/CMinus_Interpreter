module CMinusPtr

open AST
open Types
// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false  
  | AddrOf ad -> Val.Loc ad
  | LV lv->
    match lv with
    | Var s -> 
      match if Map.containsKey s mem then Map.find s mem else raise UndefinedSemantics with
      | Int i -> Int i
      | Bool b -> Bool b
      | Loc l -> Loc l
    | Deref d ->
      match evalExp d mem with
      | Loc adr -> if Map.containsKey adr mem then Map.find adr mem else raise UndefinedSemantics
      | _ -> raise UndefinedSemantics
  |Add (e1 , e2) ->
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Int(i1 + i2)
    | (Loc d1, Loc d2) ->
      match (evalExp (AddrOf d1) mem, evalExp (AddrOf d2) mem) with
      | (Int i1, Int i2) -> Int(i1 + i2)
      | _ -> raise UndefinedSemantics
    | (Loc d1, Int i1) ->
      match (evalExp (AddrOf d1) mem, Int i1) with
      | (Int id1, Int i1) -> Int(id1 + i1)
      | _ -> raise UndefinedSemantics
    | (Int i1, Loc d1) ->
      match (Int i1, evalExp (AddrOf d1) mem) with
      | (Int i1, Int id1) -> Int(i1 + id1)
      | _ -> raise UndefinedSemantics
    | _ -> raise UndefinedSemantics
  //
  //
  //
  | Sub (e1 , e2) ->
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Int i1, Int i2) -> Int(i1 - i2)
    | (Loc d1, Loc d2) ->
      match (evalExp (AddrOf d1) mem, evalExp (AddrOf d2) mem) with
      | (Int i1, Int i2) -> Int(i1 - i2)
      | _ -> raise UndefinedSemantics
    | (Loc d1, Int i1) ->
      match (evalExp (AddrOf d1) mem, Int i1) with
      | (Int id1, Int i1) -> Int(id1 - i1)
      | _ -> raise UndefinedSemantics
    | (Int i1, Loc d1) ->
      match (Int i1, evalExp (AddrOf d1) mem) with
      | (Int i1, Int id1) -> Int(i1 - id1)
      | _ -> raise UndefinedSemantics
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
    | (Loc l1, Loc l2) -> Bool(l1=l2)
    | _-> raise UndefinedSemantics
  | NotEq (e1, e2) -> 
    match (evalExp e1 mem, evalExp e2 mem) with
    | (Bool b1, Bool b2) -> Bool(b1<>b2)
    | (Int i1, Int i2) -> Bool(i1<>i2)
    | (Loc l1, Loc l2) -> Bool(l1<>l2)
    | _-> raise UndefinedSemantics  
  //| _ -> Int (-1) // TODO: fill in the remaining cases.

// Note: You may define more functions.

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  | Assign (lv, e) ->
    match lv with
    | Var lv ->
      Map.add lv (evalExp e mem) mem 
    | Deref lv ->
      let lvv = evalExp lv mem
      let ee = evalExp e mem
      match lvv with
      | Val.Loc l -> Map.add l ee mem
      | _ -> raise UndefinedSemantics
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
