module parser
open mparsec

type Id = Id of string

module AST =
    type Expr =
    | EString of string
    | ENumber of float
    | EId of Id
    | EList of Expr list
    | EQuotedList of Expr list

open AST

let exprAction, expr = refl<Expr> ()

let quotedListCall = betweenStr "'(" (sepBy spaces1 expr) ")" ==> EQuotedList

let listExpr = betweenChar '(' (sepBy spaces1 expr) ')' ==> EList

choice [ 
    pfloat ==> ENumber
    pChar '"' >-> anyStr <-< pChar '"' ==> EString >~> "EString"
    listExpr
    quotedListCall
    anyStr ==> (Id >> EId) >~> "EId"
] |> exprAction





type RuntimeValue =
| RNumber of float
| RString of string
| RUnit
| RFunction of args : Id list * body : AST.Expr

let (|RNumbers|_|) (exprs : Expr list) =
    if List.forall (function ENumber _ -> true | _ -> false) exprs then
        List.map (function ENumber n -> n | _ -> failwith "impossible") exprs |> Some
    else None

let (|EFunCall|_|) = function
| EList (EId id :: args) -> Some (id, args)
| _-> None

let (|IsFuncDef|_|) ast =
    let rec isFunc args = function
    | [EList _ as body] -> RFunction (args |> List.rev, body) |> Some
    | EId arg :: es -> isFunc (arg::args) es
    | _ -> None
    match ast with
    | EList (EId (Id "define") :: (EId id) :: es) -> isFunc [] es |> Option.map (fun func -> id, func)
    | _ -> None

let rec evaluate (environment:Map<Id, RuntimeValue>) (ast:Expr) : RuntimeValue * Map<_,_> = 
    let evaluate' env ast = evaluate env ast |> fst
    let (|EAllNumbers|_|) exprs = 
        let evaluated = List.map (evaluate' environment) exprs
        if List.forall (function RNumber _ -> true | _ -> false) evaluated then
            List.choose (function RNumber n -> Some n | _ -> None) evaluated |> Some
        else None
    
    let (|IsFunCall|_|) fname =
        match Map.tryFind fname environment with
        | Some (RFunction (args, body)) -> Some (args, body)
        | _ -> None

    match ast with
    | ENumber n -> RNumber n, environment
    | EString s -> RString s, environment
    | EId id when environment.ContainsKey id -> environment.[id], environment
    | EFunCall (Id "define", EId name :: [value]) -> RUnit, Map.add name (evaluate' environment value) environment
    | IsFuncDef (fname, func) -> RUnit, Map.add fname func environment    
    | EFunCall (Id "+", EAllNumbers numbers) -> numbers |> List.reduce (+) |> RNumber, environment
    | EFunCall (Id "-", EAllNumbers numbers) -> numbers |> List.reduce (-) |> RNumber, environment
    | EFunCall (Id "*", EAllNumbers numbers) -> numbers |> List.reduce (*) |> RNumber, environment
    | EFunCall (Id "/", EAllNumbers numbers) -> numbers |> List.reduce (/) |> RNumber, environment
    | EFunCall (IsFunCall (args, func), expr) when args.Length = expr.Length ->
        let environment = 
            expr |> List.map (evaluate' environment)
            |> List.zip args
            |> List.fold (fun environment (name, value) -> Map.add name value environment) environment
        evaluate environment func

    | x -> failwithf "Unsupported AST %A" x


let run (txt:string) = 
    let run txt env =
        match expr <!!> txt with
        | Parsed (parsed, _) -> evaluate env parsed |> Ok
        | Failed (reason, _) -> Error reason
    txt.Replace("\r\n", "\n").Split([|'\n'|])
    |> Array.fold (fun env code -> 
        FSharp.Core.Result.bind (fun (value:RuntimeValue, env) -> printfn "%A" value; run code env) env) (Ok (RUnit, Map.empty))

"""(define add2 z (+ 2 z))
(define id i (+ 2 z))
(add2 5)
(add2 7)
(define x (+ 1 2 3 (add2 10)))
(define marcin "Marcin")
(+ x)"""
|> run


