module mparsec

open System

[<RequireQualifiedAccess>]
module StringEx =
    exception OutOfRangeException of string * index:int

    [<StructuredFormatDisplay("\"{AsString}\"")>]
    type StringEx = private { str:string; index : int }
    with 
        override s.ToString () = s.str.Substring s.index
        member m.AsString = m.ToString()

        member s.Length = s.str.Length - s.index
        member s.Position = s.index
        member s.Item with get (index) = s.str.[s.index + index]

        member s.Substring index = 
            if index <= s.Length then { s with index = s.index + index }
            else raise <| OutOfRangeException (s.ToString (), index)

    let toStringEx str = { str = str; index = 0 }
    let substring (s:StringEx) = s.Substring

    let startsWith (s:StringEx) (txt:string) = txt.StartsWith (s.ToString(), System.StringComparison.OrdinalIgnoreCase)

[<StructuredFormatDisplay("\"{AsString}\"")>]

type Result<'u> = 
| Parsed of 'u * StringEx.StringEx
| Failed of reason : string * StringEx.StringEx
with
    member r.AsString =
        match r with
        | Parsed (v, str) -> sprintf "OK (%A, remaining = %O)" v str
        | Failed (reason, str) -> sprintf "Failed (reason = %s, position = %d, remaining = %O)" reason str.Position str

[<RequireQualifiedAccess>]
module Result =
    let isParsed (r:Result<'u>) = match r with Parsed _ -> true | _ -> false
    let isFailed (r:Result<'u>) = match r with Failed _ -> true | _ -> false
    let bind (binder:'t*StringEx.StringEx->Result<'u>) (bind: Result<'t> ) : Result<'u> = 
        match bind with 
        | Parsed (v, txt) -> binder (v, txt)
        | Failed (reason, txt) -> Failed (reason, txt)
    let map (mapper:'t->'u) (v: Result<'t> ) : Result<'u> = 
        match v with 
        | Parsed (v, txt) -> Parsed(mapper v, txt)
        | Failed (reason, txt) -> Failed (reason, txt)
    let defaultValue (def) (result:Result<'u>)  = 
        match result with
        | Parsed (v, txt) -> (v, txt)
        | Failed _ -> def

type Parser<'u> = { f: StringEx.StringEx -> Result<'u>; name : string }
let (>~>) (p:Parser<'a>) name : Parser<'a> = { p with name = name }

let getNames parsers = parsers |> List.map (fun x->x.name) |> String.concat ", "

let pChar ch : Parser<_> =
    { name = sprintf "pChar %c" ch
      f = fun txt -> if txt.Length > 0 && txt.[0] = ch then Parsed (ch, StringEx.substring txt 1)
                     else Failed (sprintf "Cannot match '%c'" ch, txt) }

let pString str : Parser<_> =
    { name = sprintf "pString %s" str
      f = fun txt -> 
        if StringEx.startsWith txt str then Parsed (str, txt.Substring str.Length)
        else Failed (sprintf "string doesnt start with '%s'" str, txt) }
    
let any (parsers: Parser<'a> list) : Parser<_> =
    let rec any (parsers: Parser<'a> list) txt =
        match parsers with
        | [] -> Failed (sprintf "Not found any of pattern", txt)
        | x::xs ->
            match x.f txt with
            | Parsed _ as x -> x
            | Failed _ -> any xs txt

    { name = sprintf "any (%s)" (getNames parsers)
      f = fun txt -> any parsers txt}

let anyChar = 
    { name = "anyChar"
      f = fun txt -> (['A'..'Z'] @ ['a'..'z'] |> List.map pChar |> any).f txt }
let anyNumber = 
    { name = "anyNumber"
      f = fun txt -> (['0'..'9'] |> List.map pChar |> any).f txt }

let all (parser:Parser<'a>) : Parser<_> =
    let rec all txt agg =
        match parser.f txt with
        | Parsed (v, txt) -> all txt (v :: agg)
        | Failed (reason, txt) as failed ->
            if List.isEmpty agg then Failed (reason, txt)
            else Parsed (List.rev agg, txt)
    { name = sprintf "all (%s)" parser.name
      f = fun txt -> all txt [] }
    

let choice (parsers:Parser<'a> list) : Parser<_> =
    { name = sprintf "choice (%s)" <| getNames parsers
      f = fun txt -> 
            match Seq.map (fun parser->parser.f txt) parsers |> Seq.tryFind (Result.isParsed) with
            | Some (Parsed _ as p) -> p
            | _ -> Failed (sprintf "failed to choice from %s" <| getNames parsers, txt) }
    
let (>->) (a:Parser<'a>) (b:Parser<'b>) : Parser<'b> = 
    { name = a.name + ">->" + b.name
      f = a.f >> Result.bind (snd>>b.f) }
let (<-<) (a:Parser<'a>) (b:Parser<'b>) : Parser<'a> =
    { name = a.name + "<-<" + b.name
      f = a.f >> Result.bind (fun (v1, txt) -> b.f txt |> Result.map (fun _ -> v1)) }
    
let (<->) (a:Parser<'a>) (b:Parser<'b>) : Parser<'a*'b> =
    { name = a.name + "<->" + b.name
      f = a.f >> Result.bind (fun (v1, txt) -> b.f txt |> Result.map (fun v2 -> v1, v2)) }
    
let (!!) (parser:Parser<_>) name = { parser with name = name }

let (==>) (a:Parser<'a>) (map:'a->'b) = 
    { name = a.name
      f = a.f >> Result.map map }
let between a b a' : Parser<_> = a >-> b <-< a'
let betweenChar a b a' = pChar a >-> b <-< pChar a'
let sepBy sep a = all (a <-< sep) <-> a ==> fun (a, b) -> a @ [b]
let anyStr = all anyChar ==> (Array.ofList >> System.String)

let spaces1 = all (pChar ' ')
let spaces txt = (all (pChar ' ')).f txt |> Result.defaultValue ([], txt) |> Some

let refl<'a> () =
    let mutable (b:Parser<'a>) = Unchecked.defaultof<_>
    let c = ref 0;
    (fun x-> b<-x), 
    { name = "refl" 
      f = fun txt -> c:=!c+1; if !c < 1000 then let r = b.f txt in c:= !c-1; r else Failed ("stack-over-flow", txt)}


let pfloat = 
    let parse (p:char list) = p |> Array.ofList |> System.String |> System.Convert.ToDouble
    choice [
        all anyNumber <-< pChar '.' <-> all anyNumber ==> fun (a,b) -> a @ ['.'] @ b |> parse 
        all anyNumber <-< pChar '.' ==> parse
        all anyNumber ==> parse ] >~> "pfloat"

let pint = 
    let parse (p:char list) = p |> Array.ofList |> System.String |> System.Convert.ToInt64
    all anyNumber ==> parse >~> "pint"

let pbool = 
    choice [
        pString "true" ==> fun _ -> true
        pString "false" ==> fun _ -> false ] >~> "pbool"


let parse (p:Parser<_>) txt = txt |> StringEx.toStringEx |> p.f
let (<!!>) p txt = parse p txt
