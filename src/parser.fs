module parser
open mparsec

module AST =
    type Expr =
    | Add of Expr list
    | Mul of Expr list
    | Div of Expr list
    | Sub of Expr list
    | EString of string
    | ENumber of float

open AST

let exprAction, expr = refl<Expr> ()

let mathExpr ch op = betweenChar '(' (pChar ch <-< spaces1 >-> sepBy spaces1 expr) ')' ==> op
let addExpr = mathExpr '+' Add
let mulExpr = mathExpr '*' Mul
let divExpr = mathExpr '/' Div
let subExpr = mathExpr '-' Sub

choice [ 
    pfloat ==> ENumber
    pChar '"' >-> anyStr <-< pChar '"' ==> EString
    pChar '\'' >-> anyStr <-< pChar '\'' ==> EString
    addExpr    
    mulExpr    
    divExpr    
    subExpr    
] |> exprAction

sepBy spaces1 (pChar 'a') <!!> "a    a"


addExpr <!!> "(+ 1 2)"



type RuntimeValue =
| RNumber of float
| RString of string
| RFunction of args : RuntimeValue * body : AST.Expr

let (|RNumbers|_|) (exprs : Expr list) =
    if List.forall (function ENumber _ -> true | _ -> false) exprs then
        List.map (function ENumber n -> n | _ -> failwith "impossible") exprs |> Some
    else None

let rec eval (ast:AST.Expr) = 
    match ast with    
    | ENumber n -> RNumber n
    | EString s -> RString s
    | Add (RNumbers numbers) -> List.reduce (+) numbers |> RNumber
    | Sub (RNumbers numbers) -> List.reduce (-) numbers |> RNumber
    | Div (RNumbers numbers) -> List.reduce (/) numbers |> RNumber
    | Mul (RNumbers numbers) -> List.reduce (*) numbers |> RNumber
    | x -> failwithf "Unsupported AST %A" x
let parse txt = None


