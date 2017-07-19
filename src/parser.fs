module parser
open mparsec

type Id = Id of string

module AST =
    type Expr =
    | EFunCall of Id * Expr list
    | EString of string
    | ENumber of float
    | EId of Id
    | EList of Expr list
    | EQuotedList of Expr list

open AST

let exprAction, expr = refl<Expr> ()

let funCall = betweenChar '(' (anyStr <-< spaces1 <-> sepBy spaces1 expr) ')' 
                ==> fun (funName, expr) -> EFunCall(Id funName, expr)

let quotedListCall = betweenStr "'(" (sepBy spaces1 expr) ")" 
                        ==> EQuotedList


choice [ 
    pfloat ==> ENumber
    pChar '"' >-> anyStr <-< pChar '"' ==> EString >~> "EString"
    quotedListCall
    funCall
    anyStr ==> (Id >> EId) >~> "EId"
] |> exprAction


expr <!!> """(add '(+ 1 2))"""

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


