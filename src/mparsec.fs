module mparsec
type Result<'u> = ('u * string) option
type Parser<'u> = string -> Result<'u>

let pChar ch (txt:string) =
    if txt.Length > 0 && txt.[0] = ch then Some (ch, txt.Substring 1)
    else None

let pString str (txt:string) =
    if txt.StartsWith (str, System.StringComparison.InvariantCultureIgnoreCase) then Some (str, txt.Substring str.Length)
    else None

let any (a: Parser<'a> list) txt =
    let rec any (a: Parser<'a> list)  =
        match a with
        | [] -> None
        | x::xs ->
            match x txt with
            | Some x -> Some x
            | None -> any xs
    any a

let anyChar = ['A'..'Z'] @ ['a'..'z'] |> List.map pChar |> any

let all (a:Parser<'a>) (txt:string) =
    let rec all (a:Parser<'a>) (txt:string) agg =
        match a txt with
        | Some (v, txt) -> all a txt (v::agg)
        | None ->
            if List.isEmpty agg then None
            else Some (List.rev agg, txt)
    all a txt []

let choice (a:Parser<'a> list) txt=
    match Seq.map (fun f->f txt) a |> Seq.tryFind (Option.isSome) with
    | Some (Some s) -> Some s
    | _ -> None

let (>->) (a:Parser<_>) (b:Parser<_>) = a >> Option.bind (snd>>b)
let (<-<) (a:Parser<_>) (b:Parser<_>) = 
    a >> Option.bind (fun (v1, txt) -> b txt |> Option.map (fun (_, txt) -> v1, txt))
let (<->) (a:Parser<_>) (b:Parser<_>) = 
    a >> Option.bind (fun (v1, txt) -> b txt |> Option.map (fun (v2, txt) -> (v1, v2), txt))
let (==>) (a:Parser<'a>) (map:'a->'b) = a >> Option.map (fun (v, txt) -> map v, txt)
let between a b a' = a >-> b <-< a'
let betweenChar a b a' = pChar a >-> b <-< pChar a'
let sepBy sep a = all (a <-< sep) <-> a ==> fun (a, b) -> a @ [b]
let sepByChar sep a txt = 
    match (all (a <-< pChar sep) <-> a ==> fun (a, b) -> a @ [b]) txt with
    | None -> a txt |> Option.map (fun (v, txt) -> [v], txt)
    | s -> s 
let anyStr = all anyChar ==> (Array.ofList >> System.String)

let spaces1 = all (pChar ' ')
let spaces txt = all (pChar ' ') txt |> Option.defaultValue ([], txt) |> Some

let ref<'a> () =
    let mutable (b:Parser<'a>) = Unchecked.defaultof<_>
    (fun x-> b<-x), (fun txt -> b txt)

let f = betweenChar '(' (sepByChar ',' (spaces >-> anyStr <-< spaces)) ')'

let parse txt = f txt
