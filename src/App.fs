module calc

open mparsec
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let init () =
    let doc = Browser.document
    let button1 = doc.getElementsByTagName_button().[0]
    let textbox1 = doc.getElementsByTagName_input().[0]
    let div1 = doc.getElementsByTagName_div().[0]

    button1.addEventListener_click(fun _ -> 
        printfn "click %s" textbox1.value 
        let v = parse textbox1.value
        printfn "parsed %A" v
        
        div1.innerHTML <- sprintf "%A" v;null)


    ()

init ()