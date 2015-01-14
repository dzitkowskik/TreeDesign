module Translation

open Ast
open DesignTree

let rec translateStm = function
    | Asg(e1, e2) -> 
        let left = translateExp e1 
        let right = translateExp e2
        Node("Asg", [left; right])
    | AsgArr(s, e1, e2) -> 
        let left = Node(s.ToString(), [])
        let middle = translateExp e1 
        let right = translateExp e2
        Node("AsgArr", [left; middle; right])
    | PrintLn(e) -> Node("PrintLn", [translateExp e])
    | Seq(l) -> Node("Seq", List.map translateStm l)
    | While(e, s) ->
        let left = translateExp e
        let right = translateStm s
        Node("While", [left; right])
    | Block(l, s) ->
        let left = List.map translateDec l
        let right = translateStm s
        Node("Block", List.append left [right])
    | Call(s, l) ->
        let left = Node(s.ToString(), [])
        let right = List.map translateExp l
        Node("Call", left::right)
    | IfThen(e, s) ->
        let left = translateExp e
        let right = translateStm s
        Node("IfThen", [left; right])
    | IfElse(e, s1, s2) ->
        let left = translateExp e
        let middle = translateStm s1
        let right = translateStm s2
        Node("IfElse", [left; middle; right])
    | Ret(e) -> Node("Ret", [translateExp e])
and translateExp = function
    | Int(i) -> Node("Int", [Node(i.ToString(),[])])
    | Bool(b) -> Node(b.ToString(), [])
    | String(s) -> Node(s.ToString(), [])
    | Var(a) -> Node("Var", [Node(a.ToString(),[])])
    | ContOf(e) -> Node("ContOf", [translateExp e])
    | Apply(s, l) -> 
        let left = Node(s.ToString(), [])
        let right = List.map translateExp l
        Node("Apply", left::right)
    | Arr(s, e) ->
        let left = Node(s.ToString(), [])
        let right = translateExp e
        Node("Arr", [left; right])
    | Field(s1, s2) ->
        let left = Node(s1.ToString(), [])
        let right = Node(s2.ToString(), [])
        Node("Field", [left; right])
    | Const(s) -> Node(s.ToString(), [])
and translateDec = function
    | VarDec(s, e) -> 
        let left = Node(s.ToString(), [])
        let right = translateExp e
        Node("VarDec", [left; right])
    | VarArr(s, e1, e2) ->
        let left = Node(s.ToString(), [])
        let middle = translateExp e1
        let right = translateExp e2
        Node("VarArr", [left; middle; right])
    | Procedure(s, l, st) ->
        let left = Node(s.ToString(), [])
        let middle = List.map translateExp l
        let right = translateStm st
        Node("Procedure", List.append (left::middle) [right])
    | RecProcedure(s, l, st) ->
        let left = Node(s.ToString(), [])
        let middle = List.map translateExp l
        let right = translateStm st
        Node("RecProcedure", List.append (left::middle) [right])