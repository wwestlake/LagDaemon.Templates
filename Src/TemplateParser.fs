namespace LagDaemon.Templates

module TemplateParser =

    open FParsec
    open System.IO

    type Replacer = Replacer of (string -> string list -> string )
    type Command = Command of string
    type Param = Param of string list
    type ReplacementMap = ReplacementMap of Map<string, Replacer>
        with static member create () = ReplacementMap Map.empty

    type Tag = {
        command: Command
        param: Param option
    }
        with static member create command param = { command = Command command; param = Some <| Param param }
    
    type AST =
        | Text of string
        | Tag of Tag
    
    type UserState = unit
    
    type Parser<'a> = Parser<'a, UserState>

    let str = pstring
    let ws = spaces
    let ws1 = spaces1
    let str_ws s = pstring s .>> ws
    
    let text : Parser<_> =  many1Satisfy (isNoneOf "{}") |>> Text
    
    let parameter : Parser<_> =
        let isparameterFirstChar c = isLetter c || c = '_'
        let isparameterChar c = isLetter c || isDigit c || isAnyOf "!@#$%^&*[]:\\\";'<>,.?/_ " c 
    
        many1Satisfy2L isparameterFirstChar isparameterChar "identifier"
        .>> ws // skips trailing whitespace
    
    let paramList : Parser<_> = str "(" >>. ws >>. sepBy parameter (str_ws ",") .>>  str_ws ")" |>> (fun x -> Param <| x)
    
    let innerCommand = ws >>. parameter .>> ws 
    
    
    let commandParam = between (str_ws "{|") (str "|}") (innerCommand .>>. opt paramList) 
                            |>> (fun (x,y) -> 
                                    Tag { 
                                        command = Command x
                                        param = y
                                    }
                                )
    
    let command = many <| choice [ commandParam; text] 
    
    let parseString p input =
        runParserOnString p () "test" input
    
    let parseFile p filename =
            runParserOnFile p () filename System.Text.Encoding.UTF8

    let runReplacer replacer cmd parameters = 
            let (Replacer f) = replacer
            f cmd parameters

    let add replacementMap key f =
        let (ReplacementMap map) = replacementMap
        ReplacementMap <| Map.add key f map

    let addList replacementMap list =
        let rec loop map rest =
            match rest with
            | [] -> map
            | head::tail ->
                let (cmd, f) = head
                loop (add map cmd f) tail
        loop replacementMap list

    let find replacementMap key =
        let (ReplacementMap map) = replacementMap
        Map.find key map


    let processDoc parser input replacementMap = 
        let rec loop rest text =
            match rest with
            | [] -> text
            | head::tail ->
                match head with
                | Text t -> loop tail (text + t)
                | Tag tag -> 
                    let (Command cmd) = tag.command
                    match tag.param with
                    | Some (Param p) -> 
                        let repl = runReplacer (find replacementMap cmd) cmd p
                        loop tail (text + repl)
                    | None -> 
                        let repl = runReplacer (find replacementMap cmd) cmd []
                        loop tail (text + repl)
    
    
        match parser command input with
        | Success (r,_,_) -> (loop r "")
        | Failure (_,msg,_) -> msg.ToString()
    
    let processString input map = 
        let parser = processDoc parseString 
        try 
            Result.Ok <| parser input map
        with
        | excn -> Result.Error <| excn.ToString()
            
    let processFile filename map =
        let parser = processDoc parseFile
        try 
            Result.Ok <| parser filename map
        with
        | excn -> Result.Error <| excn.ToString()


