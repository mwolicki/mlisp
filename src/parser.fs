module parser
open mparsec

module AST =
    type Expr =
    | Add of Expr list
    | Mul of Expr list
    | Div of Expr list
    | Sub of Expr list
    | EInt of int64


open AST

let exprAction, expr = refl<Expr> ()

let mathExpr ch op = betweenChar '('  (pChar ch <-< spaces1 >-> sepBy spaces1 expr) ')' ==> op
let addExpr = mathExpr '+' Add
let mulExpr = mathExpr '*' Mul
let divExpr = mathExpr '/' Div
let subExpr = mathExpr '-' Sub

choice [ 
    pint ==> EInt
    addExpr    
    mulExpr    
    divExpr    
    subExpr    
] |> exprAction


expr "(+ 1 1 1 (+ 1 2))"


let parse txt = None

