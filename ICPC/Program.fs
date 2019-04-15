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

let commaSprinkler input =
    failwith "Not implemented"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
