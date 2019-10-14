# LagDaemon.Templates
An F# utility for processing templates


```fsharp

open System
open LagDaemon.Templates.TemplateParser

[<EntryPoint>]
let main argv =
    
    let replacers : (string * Replacer) list = [
        ("date", Replacer (fun cmd prms -> 
            match prms with
            | [] -> DateTime.Now.Date.ToString()
            | h::_ -> DateTime.Now.Date.ToString(h)
            )
        )

        ("time", Replacer (fun cmd prms -> 
            match prms with
            | [] -> DateTime.Now.TimeOfDay.ToString()
            | h::_ -> DateTime.Now.TimeOfDay.ToString(h)
            )
        )
        
        ("datetime", Replacer (fun cmd prms -> 
            match prms with
            | [] -> DateTime.Now.ToString()
            | h::_ -> DateTime.Now.ToString(h)
            )
        )
    
        ("user", Replacer (fun cmd prms -> "Dave" ))
        ("system", Replacer (fun cmd prms -> "LagDaemon MUD" ))
    ]


    let map = addList (ReplacementMap.create()) replacers
    
    let template = @"

        Hello {|user|},
    
        Today, {|date(dd/MM/yy)|}, we {had} some interesting things happen.
        I am writing this at {|time(hh\:mm\:ss)|} and it will take me
        a few minutes. And (parentetically speaking, braces not allowed).
            Bill - {|datetime(dddd, dd MMMM yyyy)|}
            {|system()|}
    
            *&^%$#^%$*&^%)*)(*^*&^^%$##@$#*&^(*)(*&)&_&(*
    "

    let text = processString template map

    printfn "%s" text

    0 // return an integer exit code




```
