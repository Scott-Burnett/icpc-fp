module ICPC
open System

let stringToList (input:string) = 
    let rec inner i (newWord:string) (newList:List<string>) = 
        match i < input.Length with 
        |true -> newList
        |false -> match input.[i .. i] with 
                    |" "|"."|"," -> inner (i+1) "" ([input.[i..i]] @ [newWord] @ newList)
                    |s -> inner (i+1) (newWord + s) newList
    inner 1 "" []

let getCommaBeforeWords (input:List<string>) =
    let rec inner state (input:List<string>) (output:list<string>) =
        match state with
        |0 -> match input with
            |(" "::t)|(","::t)|("."::t)|[] -> failwith "incorrect string format"
            |s::t -> inner 1 t
        |1 -> match input with
            |","::t -> inner 2 t output
            |"."::t -> inner 5 t output
            |" "::t -> inner 6 t output
            |_ -> failwith "incorrect string format" 
        |2 -> match input with 
            |" "::t -> inner 3 t output
            |_ -> failwith "incorrect string format"
        |3 -> match input with  
            |","::t|" "::t|"."::t|[] -> failwith "incorrect string format"
            |s::t -> inner 4 t (s @ output)
        |4 -> match input with
            |","::t -> inner 2 t output
            |"."::t -> inner 5 t output
            |" "::t -> inner 6 t output
            |_ -> failwith "incorrect string format"
        |5 -> match input with 
            |" "::t -> inner 6 t output
            |[] -> inner 7 input output
            |_ -> failwith "incorrect string format" 
        |6 -> match input with 
            |" "::t|","::t|"."::t|[] -> failwith "incorrect string format"
            |s::t -> inner 1 t output
        |7 -> output
    inner 0 input []


let commaSprinkler input =
    failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
