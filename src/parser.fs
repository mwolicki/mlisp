module parser
open mparsec
open System

type Id = Id of string

module AST =
    type Expr =
    | EString of string
    | ENumber of float
    | EId of Id
    | EList of Expr list
    | EQuotedList of Expr list
    | ENop
    
    let rec tryRemoveNop = function
    | EString _
    | ENumber _
    | EId _ as e -> e
    | EList es -> List.map tryRemoveNop es |> List.choose (function ENop -> None | x -> Some x) |> EList
    | EQuotedList es -> List.map tryRemoveNop es |> List.choose (function ENop -> None | x -> Some x) |> EQuotedList
    | ENop -> ENop

open AST

let exprAction, expr = refl<Expr> ()
let comment = spaces >-> pChar ';' <-< all(anyASCII) <-< pChar '\n' >~> "comment"
let quotedListCall = spaces >-> betweenStr "'(" (sepBy spaces1 expr) ")" ==> EQuotedList
let listExpr = spaces >-> betweenChar '(' (sepBy spaces1 expr) ')' ==> EList

choice [ 
    pfloat ==> ENumber
    pChar '"' >-> anyStr <-< pChar '"' ==> EString >~> "EString"
    listExpr
    listExpr <-< comment
    quotedListCall
    comment ==> fun _ -> ENop    
    anyStr ==> (Id >> EId) >~> "EId"
] |> exprAction

type RuntimeValue =
| RNumber of float
| RString of string
| RUnit
| RList of RuntimeValue list
| RFunction of args : Id list * body : AST.Expr
with override rv.ToString () =
        match rv with
        | RNumber v -> v.ToString ()
        | RString s -> s
        | RUnit -> "unit"
        | RList rs -> "[" + (rs |> List.map (fun r -> r.ToString()) |>String.concat ", ") + "]"
        | RFunction (args, _) -> "(@func " + (args |> List.map (fun (Id i) -> i) |> String.concat ", ") + ")"

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
    | EFunCall (Id "+", EAllNumbers nums) -> nums |> List.reduce (+) |> RNumber, environment
    | EFunCall (Id "-", EAllNumbers nums) -> nums |> List.reduce (-) |> RNumber, environment
    | EFunCall (Id "*", EAllNumbers nums) -> nums |> List.reduce (*) |> RNumber, environment
    | EFunCall (Id "/", EAllNumbers nums) -> nums |> List.reduce (/) |> RNumber, environment
    | EFunCall (Id "^", EAllNumbers nums) -> nums |> List.reduce (fun a b -> System.Math.Pow(a,b)) |> RNumber, environment
    | EFunCall (Id "log", [ENumber num]) -> log num |> RNumber, environment
    | EFunCall (Id "log10", [ENumber num]) -> log10 num |> RNumber, environment
    | EFunCall (Id "abs", EAllNumbers nums) -> nums |> List.map (abs >> RNumber) |> RList, environment    
    | EFunCall (IsFunCall (args, func), expr) when args.Length = expr.Length ->
        let environment = 
            expr |> List.map (evaluate' environment)
            |> List.zip args
            |> List.fold (fun environment (name, value) -> Map.add name value environment) environment
        evaluate environment func
    | ENop -> RUnit, environment
    | EQuotedList es -> es |> List.mapFold (evaluate) environment |> fun (ps, environment) -> RList ps, environment
    | x -> failwithf "Unsupported AST %A" x

let run (txt:string) = 
    match all expr <!!> (txt + "\n") with
    | Parsed (parsed, _) -> evaluate Map.empty (EQuotedList parsed |> AST.tryRemoveNop) |> Ok
    | Failed (reason, _) -> Error reason
    
"""(define addTwoNumbers a b (+ a b)) ; da
(define add2 z (addTwoNumbers 2 z))
(define x (* 10 (+ 1 2 3 (add2 10))))
(+ 1 2 (add2 5) x)"""
|> run

