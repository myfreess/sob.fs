

open System

type Lexpr = | Var of string
             | Const of bool
             | And of Lexpr * Lexpr
             | Or of Lexpr * Lexpr
             | Not of Lexpr

// type Cnf = Lexpr (* 简单的别名，但是当使用Cnf时，实际数据的结构应该为cnf  *)

[<RequireQualifiedAccess>]
type CnfAtom = | Var of string
               | Const of bool
               | Not of CnfAtom

type CnfClause = List<CnfAtom>
type Cnf       = List<CnfClause>


// 最简单的暴力求解

let rec freeVariable (e : Lexpr) : Option<string> =
    match e with
    | Var(s)      -> Option.Some(s)
    | Const(_)    -> Option.None
    | And(e1, e2) -> 
        let pfv = freeVariable e1
        match pfv with
        | None -> freeVariable e2
        | _    -> pfv
    | Or(e1, e2)  -> 
        let pfv = freeVariable e1
        match pfv with
        | None -> freeVariable e2
        | _    -> pfv
    | Not(e)      -> freeVariable e

let rec guessVariable (fvar : string) value e =
    // 尝试为一个自由变量赋值，并给出更新后的表达式
    let guess = guessVariable fvar value
    match e with
    | Var(s) -> 
        if s = fvar
        then Const(value)
        else e
    | Not(e) -> Not(guess e)
    | Or(e1,e2) -> Or(guess e1, guess e2)
    | And(e1, e2) -> And(guess e1, guess e2)
    | Const(_) -> e

let rec simplify e =
    match e with
    | Const(b) -> e
    | Var(v)   -> e
    | Not(e1)  -> match e1 with 
                  | Const(b) -> Const(``not`` b) 
                  | _        -> e1
    | Or(x, y) -> 
        let es = List.filter (fun x -> match x with | Const(false) -> false | _ -> true) [simplify x; simplify y]
        if List.contains (Const(true)) es
        then Const(true)
        else match es with
             | [] -> Const(false)
             | [e1] -> e1
             | [e1; e2] -> Or(e1, e2)
             | _        -> e // impossible branch
    | And(x, y) ->
        let es = List.filter (fun x -> match x with | Const(true) -> false | _ -> true) [simplify x; simplify y]
        if List.contains (Const(false)) es
        then Const(false)
        else match es with
             | [] -> Const(true)
             | [e1] -> e1
             | [e1; e2] -> And(e1, e2)
             | _ -> e


let unConst x =
    match x with
    | Const(true) -> Option.Some(true)
    | _           -> Option.None

let rec satisfiable expr = 
    match freeVariable expr with
    | None -> unConst expr
    | Some(v) -> 
        let trueGuess  = simplify (guessVariable v true  expr)
        let falseGuess = simplify (guessVariable v false expr)
        match satisfiable trueGuess with
        | None -> satisfiable falseGuess
        | _    -> Some(true)



    







[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
