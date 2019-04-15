module ICPC
open System
open System.Diagnostics
open System

let stringToList (input:string) = 
    let rec inner i (newWord:string) (newList:List<string>) = 
        match i < input.Length with 
        |true -> newList
        |false -> match input.[i .. i] with 
                    |" "|"."|"," -> inner (i+1) "" ([input.[i..i]] @ [newWord] @ newList)
                    |s -> inner (i+1) (newWord + s) newList
    inner 1 "" []

let ListToString (input:List<string>) =  
    let rec inner (input:List<string>) newString =
        match input with
            |h::t -> inner t (newString + h)
            |[] -> newString
    inner input

let listStringIsValid (input:List<string>) =
    let rec inner state (input:List<string>) =
        match state with
        |0 -> match input with
            |(" "::t) -> inner -1 [] 
            |(","::t) -> inner -1 [] 
            |("."::t) -> inner -1 [] 
            |[] -> inner -1 [] 
            |s::t -> inner 1 t 
        |1 -> match input with
            |","::t -> inner 2 t 
            |"."::t -> inner 5 t 
            |" "::t -> inner 6 t 
            |_ -> inner -1 [] 
        |2 -> match input with 
            |" "::t -> inner 3 t 
            |_ -> inner -1 [] 
        |3 -> match input with  
            |","::t -> inner -1 [] 
            |" "::t -> inner -1 [] 
            |"."::t -> inner -1 [] 
            |[] -> inner -1 [] 
            |s::t -> inner 4 t 
        |4 -> match input with
            |","::t -> inner 2 t 
            |"."::t -> inner 5 t 
            |" "::t -> inner 6 t 
            |_ -> inner -1 [] 
        |5 -> match input with 
            |" "::t -> inner 6 t 
            |[] -> inner 7 input 
            |_ -> inner -1 [] 
        |6 -> match input with 
            |" "::t -> inner -1 [] 
            |","::t -> inner -1 [] 
            |"."::t -> inner -1 [] 
            |[] -> inner -1 [] 
            |s::t -> inner 1 t 
        |7 -> true
        |_ -> false
    inner 0 input 

let getCommaBeforeWords (input:List<string>) =
    let rec inner state (input:List<string>) (output:list<string>) =
        match state with
        |0 -> match input with
            |(" "::t) -> inner -1 [] []
            |(","::t) -> inner -1 [] []
            |("."::t) -> inner -1 [] []
            |[] -> inner -1 [] []
            |s::t -> inner 1 t output
        |1 -> match input with
            |","::t -> inner 2 t output
            |"."::t -> inner 5 t output
            |" "::t -> inner 6 t output
            |_ -> inner -1 [] []
        |2 -> match input with 
            |" "::t -> inner 3 t output
            |_ -> inner -1 [] []
        |3 -> match input with  
            |","::t -> inner -1 [] []
            |" "::t -> inner -1 [] []
            |"."::t -> inner -1 [] []
            |[] -> inner -1 [] []
            |s::t -> inner 4 t ([s] @ output)
        |4 -> match input with
            |","::t -> inner 2 t output
            |"."::t -> inner 5 t output
            |" "::t -> inner 6 t output
            |_ -> inner -1 [] []
        |5 -> match input with 
            |" "::t -> inner 6 t output
            |[] -> inner 7 input output
            |_ -> inner -1 [] []
        |6 -> match input with 
            |" "::t -> inner -1 [] []
            |","::t -> inner -1 [] []
            |"."::t -> inner -1 [] []
            |[] -> inner -1 [] []
            |s::t -> inner 1 t output
        |7 -> output
        |_ -> failwith "incorrect string format"
    inner 0 input []

let getCommaAfterWords (input:List<string>) =
    let rec inner state (input:List<string>) (output:list<string>) (word:string) =
        match state with
        |0 -> match input with
            |(" "::t) -> inner -1 [] [] ""
            |(","::t) -> inner -1 [] [] ""
            |("."::t) -> inner -1 [] [] ""
            |[] -> inner -1 [] [] ""
            |s::t -> inner 1 t output s
        |1 -> match input with
            |","::t -> inner 2 t ([word] @ output) ""
            |"."::t -> inner 5 t output word
            |" "::t -> inner 6 t output word
            |_ -> inner -1 [] [] ""
        |2 -> match input with 
            |" "::t -> inner 3 t output word
            |_ -> inner -1 [] [] ""
        |3 -> match input with  
            |","::t -> inner -1 [] [] ""
            |" "::t -> inner -1 [] [] ""
            |"."::t -> inner -1 [] [] ""
            |[] -> inner -1 [] [] ""
            |s::t -> inner 1 t output s
        |5 -> match input with 
            |" "::t -> inner 6 t output word
            |[] -> inner 7 input output word
            |_ -> inner -1 [] [] ""
        |6 -> match input with 
            |" "::t -> inner -1 [] [] ""
            |","::t -> inner -1 [] [] ""
            |"."::t -> inner -1 [] [] ""
            |[] -> inner -1 [] [] ""
            |s::t -> inner 1 t output word
        |7 -> output
        |_ -> failwith "invalid string format"
    inner 0 input [] ""

let SprinkleBefore (input: List<string>) (beforeWord: string) =
    let rec inner  (input: List<string>) (output: List<string>) =
        match input with 
            |h::t -> match h = beforeWord with
                        |true -> inner t ([h] @ [","] @ output)
                        |false -> inner t ([h] @ output)
            |[] -> output
    inner input []

let sprinkleAfter (input: List<string>) (afterWord: string) =
    let rec inner  (input: List<string>) (output: List<string>) =
        match input with 
            |h::t -> match h= afterWord with 
                        |true -> inner t ([","] @ [h] @ output)
                        |false -> inner t ([h] @ output)
            |[] -> output
    inner input []

let sprinkleParse (input: List<string>) (beforeWords: List<string>) (afterWords: List<string>) =
    let rec beforeParse (beforeWords: List<string>) (output: List<string>) =
        match beforeWords with 
            |h::t -> beforeParse t (SprinkleBefore output h)
            |[] -> output
    let rec afterParse (afterWords: List<string>) (output: List<string>) =
        match afterWords with   
            |h::t -> afterParse t (sprinkleAfter output h)
            |[] -> output
    afterParse afterWords (beforeParse beforeWords input)
    
let sprinkle (input:List<string>) = 
    let rec inner (input:List<string>) =
        let newList = sprinkleParse input (getCommaBeforeWords input) (getCommaAfterWords input)
        match newList = input with
            |true -> newList
            |false -> inner newList
    inner input
             
let commaSprinkler (input:string) =
    ListToString (sprinkle (stringToList input))

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
