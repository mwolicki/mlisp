module calc

open parser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let init () =
    let doc = Browser.document
    let button1 = doc.getElementsByTagName_button().[0]
    let textbox1 = doc.getElementsByTagName_textarea().[0]
    let div1 = doc.getElementsByTagName_div().[0]

    button1.addEventListener_click(fun _ -> 
        try
            let res = 
                match parser.run textbox1.value with
                | Ok (v, _) -> sprintf "%A" v
                | Error reason -> reason
            div1.innerHTML <- res
        with e -> div1.innerHTML <- sprintf "error = %A" e.Message
        null)


    ()

init ()