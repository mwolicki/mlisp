module mparsec

open System

[<RequireQualifiedAccess>]
module StringEx =
    type Position = int
    exception OutOfRangeException of string * index:Position

    [<StructuredFormatDisplay("\"{AsString}\"")>]
    type StringEx = private { str:string; index : Position }
    with 
        override s.ToString () = s.str.Substring s.index
        member m.AsString = m.ToString()

        member s.Length = s.str.Length - s.index
        member s.Position = s.index
        member s.Item with get (index) = s.str.[s.index + index]

        member s.Substring (index:Position) = 
            if index <= s.Length then { s with index = s.index + index }
            else raise <| OutOfRangeException (s.ToString (), index)

    let toStringEx str = { str = str; index = 0 }
    let substring (s:StringEx) = s.Substring

    let startsWith (txt:string) (s:StringEx) = s.ToString().StartsWith (txt, System.StringComparison.OrdinalIgnoreCase)


type ParserName = string
type ParseStatus = { 
    Trampoline : Map<Guid*StringEx.Position, uint8>
    Text : StringEx.StringEx }

type 'a ParsedData =  'a * ParseStatus

[<StructuredFormatDisplay("\"{AsString}\"")>]
type Result<'u> = 
| Parsed of 'u ParsedData
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
    let bind (binder:'t ParsedData->Result<'u>) (bind: Result<'t> ) : Result<'u> = 
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

type Parser<'u> = 
    private { parser: ParseStatus -> Result<'u>; name : string; id:System.Guid }
    with member p.f status =
            let call c =
                let status = { status with Trampoline = Map.add (p.id, status.Text.Position) (c+1uy) status.Trampoline }
                p.parser status
            match status.Trampoline |> Map.tryFind (p.id, status.Text.Position) with
            | Some c when c > 3uy ->
                Failed ("dup", status.Text)
            | Some c -> call c
            | None -> call 0uy

let (>~>) (p:Parser<'a>) name : Parser<'a> = { p with name = name }
let parse (p:Parser<_>) txt = { Text = txt |> StringEx.toStringEx; Trampoline = Map.empty }  |> p.parser
let (<!!>) p txt = parse p txt

let getNames parsers = parsers |> List.map (fun x->x.name) |> String.concat ", "

let pChar ch : Parser<_> =
    { name = sprintf "pChar %c" ch
      id = System.Guid.NewGuid ()
      parser = fun status -> 
        let txt = status.Text
        if txt.Length > 0 && txt.[0] = ch then Parsed (ch, { status with Text = StringEx.substring txt 1 })
        else Failed (sprintf "Cannot match '%c'" ch, txt) }

let pString str : Parser<_> =
    { name = sprintf "pString %s" str
      id = System.Guid.NewGuid ()
      parser = fun status -> 
        let txt = status.Text
        if StringEx.startsWith str txt then Parsed (str, { status with Text = StringEx.substring txt str.Length })
        else Failed (sprintf "string doesnt start with '%s'" str, txt) }
    
let any (parsers: Parser<'a> list) : Parser<_> =
    let rec any (parsers: Parser<'a> list) status =
        match parsers with
        | [] -> Failed ("Not found any of pattern", status.Text)
        | x::xs ->
            match x.f status with
            | Parsed _ as x -> x
            | Failed _ -> any xs status

    { name = sprintf "any (%s)" (getNames parsers)
      id = System.Guid.NewGuid ()
      parser = fun txt -> any parsers txt}

let anyChar = 
    { name = "anyChar"
      id = System.Guid.NewGuid ()
      parser = fun txt -> (['*'..'z'] |> List.map pChar |> any).f txt }
let anyNumber = 
    { name = "anyNumber"
      id = System.Guid.NewGuid ()
      parser = fun txt -> (['0'..'9'] |> List.map pChar |> any).f txt }

let all (parser:Parser<'a>) : Parser<_> =
    let rec all status agg =
        match parser.f status with
        | Parsed (v, status) -> all status (v :: agg)
        | Failed (reason, _) as failed ->
            if List.isEmpty agg then Failed (reason, status.Text)
            else Parsed (List.rev agg, status)
    { name = sprintf "all (%s)" parser.name
      id = System.Guid.NewGuid ()
      parser = fun txt -> all txt [] }

let choice (parsers:Parser<'a> list) : Parser<_> =
    { name = sprintf "choice (%s)" <| getNames parsers
      id = System.Guid.NewGuid ()
      parser = fun status -> 
            match Seq.map (fun (p:Parser<_>)-> p.f status) parsers |> Seq.tryFind Result.isParsed with
            | Some p -> p
            | _ -> Failed (sprintf "failed to choice from %s" <| getNames parsers, status.Text) }
    
let (>->) (a:Parser<'a>) (b:Parser<'b>) : Parser<'b> = 
    { name = a.name + ">->" + b.name
      id = System.Guid.NewGuid ()
      parser = a.f >> Result.bind (snd>>b.f) }
let (<-<) (a:Parser<'a>) (b:Parser<'b>) : Parser<'a> =
    { name = a.name + "<-<" + b.name
      id = System.Guid.NewGuid ()
      parser = a.f >> Result.bind (fun (v1, txt) -> b.f txt |> Result.map (fun _ -> v1)) }
    
let (<->) (a:Parser<'a>) (b:Parser<'b>) : Parser<'a*'b> =
    { name = a.name + "<->" + b.name
      id = System.Guid.NewGuid ()
      parser = a.f >> Result.bind (fun (v1, txt) -> b.f txt |> Result.map (fun v2 -> v1, v2)) }
    
let (!!) (parser:Parser<_>) name = { parser with name = name }

let (==>) (a:Parser<'a>) (map:'a->'b) = 
    { name = sprintf "mapped-%s" a.name
      id = System.Guid.NewGuid ()
      parser = a.f >> Result.map map }
let between a b a' : Parser<_> = a >-> b <-< a'
let betweenChar a b a' = pChar a >-> b <-< pChar a'
let betweenStr a b a' = pString a >-> b <-< pString a'
let sepBy sep a = 
    { name = sprintf "sepBy (%A, %A)" sep a 
      id = System.Guid.NewGuid ()
      parser = fun status -> 
        match (all (a <-< sep)).f status with
        | Parsed (v, status) -> 
            match a.f status with
            | Parsed (v', status) -> Parsed (v @ [v'], status)
            | Failed (r, _) -> Parsed(v, status)
        | Failed _ -> 
            match a.f status with
            | Parsed (v', status) -> Parsed ([v'], status)
            | Failed (r, _) -> Failed (r, status.Text) }

    
let sepByChar sep = sepBy (pChar sep)

let anyStr = all anyChar ==> (Array.ofList >> System.String)

let spaces1 = all (pChar ' ')
let spaces txt = (all (pChar ' ')).f txt |> Result.defaultValue ([], txt) |> Some

let refl<'a> () =
    let mutable (b:Parser<'a>) = Unchecked.defaultof<_>
    let c = ref 0
    (fun x-> b<-x), 
    { name = "refl" 
      id = System.Guid.NewGuid ()
      parser = 
        fun status -> c:=!c+1; if !c < 1000 then let r = b.f status in c:= !c-1; r else Failed ("stack-over-flow", status.Text)}


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


