module ICPC
open System
open System.Diagnostics
open System

let stringToList (input:string) = 
    let rec inner i (newWord:string) (newList:List<string>) = 
        match i < input.Length with 
        |false -> List.rev ([newWord] @ newList)
        |true -> match input.[i .. i] with 
                    |" "|"."|"," -> match newWord = "" with 
                                    |true -> inner (i+1) "" (input.[i..i]::newList)
                                    |false -> inner (i+1) "" (input.[i..i]::newWord::newList)
                    |s -> inner (i+1) (newWord + s) newList
    inner 0 "" []

let ListToString (input:List<string>) =  
    let rec inner (input:List<string>) newString =
        match input with
            |h::t -> inner t (newString + h)
            |[] -> newString
    inner input

let pruneList (input:List<string>) = 
    let rec inner (input:List<string>) (output:List<string>) = 
        match input with 
            |h::t -> match List.exists (fun s -> s = h) t with 
                        |true -> inner t output
                        |false -> inner t (h::output)
            |[] -> output
    inner input []        

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
                |[""] -> inner 7 input 
                |_ -> inner -1 [] 
        |6 -> match input with 
                |" "::t -> inner -1 [] 
                |","::t -> inner -1 [] 
                |"."::t -> inner -1 [] 
                |[] -> inner -1 [] 
                |s::t -> inner 1 t 
        |7 -> Some input
        |_ -> None
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
                |"."::t -> inner 4 t output
                |" "::t -> inner 5 t output
                |_ -> inner -1 [] []
        |2 -> match input with 
                |" "::t -> inner 3 t output
                |_ -> inner -1 [] []
        |3 -> match input with  
                |","::t -> inner -1 [] []
                |" "::t -> inner -1 [] []
                |"."::t -> inner -1 [] []
                |[] -> inner -1 [] []
                |s::t -> inner 1 t (s::output)
        |4 -> match input with 
                |" "::t -> inner 5 t output
                |[""] -> inner 6 input output
                |_ -> inner -1 [] []
        |5 -> match input with 
                |" "::t -> inner -1 [] []
                |","::t -> inner -1 [] []
                |"."::t -> inner -1 [] []
                |[] -> inner -1 [] []
                |s::t -> inner 1 t output
        |6 -> Some (pruneList output)
        |_ -> None
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
                |","::t -> inner 2 t (word::output) ""
                |"."::t -> inner 4 t output word
                |" "::t -> inner 5 t output word
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
        |4 -> match input with 
                |" "::t -> inner 5 t output word
                |[""] -> inner 6 input output word
                |_ -> inner -1 [] [] ""
        |5 -> match input with 
                |" "::t -> inner -1 [] [] ""
                |","::t -> inner -1 [] [] ""
                |"."::t -> inner -1 [] [] ""
                |[] -> inner -1 [] [] ""
                |s::t -> inner 1 t output word
        |6 -> Some (pruneList output)
        |_ -> None
    inner 0 input [] ""

let canHaveCommaBefore (input:List<string>) i =
    match i < 2 with 
        |true -> false
        |false -> match input.[(i-1)..(i)] with
                    |[",";" "]|[".";" "] -> false
                    |_ -> true

let canHaveCommaAfter (input:List<string>) i =
    match i >= input.Length with 
        |true -> false
        |false -> match input.[i..i] with
                    |[","] -> false
                    |["."] -> false
                    |_ -> true

let SprinkleBefore (input: List<string>) (beforeWord: string) =
    let rec inner  (input: List<string>) (output: List<string>) i =
        match input with 
            |h::t -> match h = beforeWord with
                        |true -> match canHaveCommaBefore output i with  
                                    |false -> inner t [h] i
                                    |true -> inner t (h::" "::","::output.[1..output.Length-1]) (i+2)
                        |false -> inner t (h::output) (output.Length-1)
            |[] -> output
    inner input [] 0

let sprinkleAfter (input: List<string>) (afterWord: string) =
    let rec inner  (input: List<string>) (output: List<string>) (i:int) =
        match input with 
            |h::t -> match h = afterWord with 
                        |true -> match canHaveCommaAfter t i with 
                                    |true -> inner t (","::h::output) (i+2)
                                    |false -> inner t [h] i
                        |false -> inner t (h::output) (output.Length-1)
            |[] -> output
    inner input [] 0

let sprinkleParse (input: List<string>) (beforeWords: Option<List<string>>) (afterWords: Option<List<string>>) =
    let rec beforeParse (beforeWords: List<string>) (output: List<string>) =
        match beforeWords with 
            |h::t -> beforeParse t (SprinkleBefore output h)
            |[] -> output
    let rec afterParse (afterWords: List<string>) (output: List<string>) =
        match afterWords with   
            |h::t -> afterParse t (sprinkleAfter output h)
            |[] -> output
    match beforeWords with 
        |Some beforeWords -> match afterWords with
                                |Some afterWords -> Some (afterParse afterWords (beforeParse beforeWords input))
                                |None -> None
        |None -> None
    
let sprinkle (input:List<string>) = 
    let rec inner (input:List<string>) =
        let newList = sprinkleParse input (getCommaBeforeWords input) (getCommaAfterWords input)
        match newList with
            |(Some value) -> match value = input with
                                |true -> Some (ListToString value)
                                |false -> inner value
            |None -> None     
    inner input
             
let commaSprinkler (input:string) =
    sprinkle (stringToList input)

//let rec longest (words:string) long current count=
//    match count <= words.Length with
//    |true ->    let current = words.IndexOf(' ')
//                let long = words.IndexOf(' ',current+1)
//                match long >= 0 with 
//                |true ->     match long >= current with 
//                             |true -> longest words long long (count+1)
//                             |_ -> longest words long current (count+1)
//               |_ -> let long = 0
//                    match long >= current with 
//                   |true -> longest words long long (count+1)
//                  |_ -> longest words long current (count+1)
//|_ -> long+1
  
//let checksplit words current=
  //  let bob = longest words 0 0 0
    
let tolistthing (input:string) = 
    let rec inner i (newWord:string) (newList:List<string>) = 
        match i < String.length(input) with 
        |false -> List.rev ([newWord] @ newList)
        |true -> match input.[i .. i] with 
                    |" " -> match newWord = "" with 
                                    |true -> inner (i+1) "" ([input.[i..i]] @ newList)
                                    |false -> inner (i+1) "" ([input.[i..i]] @ [newWord] @ newList)
                    |s -> inner (i+1) (newWord + s) newList
    inner 0 "" []
        
        
    
let rec lengthOfthing words longest =
    match words with
    |head::tail -> match (head:string) with 
                   |" " -> lengthOfthing tail longest//head = The     tail = " " "Yang..."
                   |_ -> let long = (String.length(head))
                         match long > longest with
                         |true -> lengthOfthing tail long
                         |_ -> lengthOfthing tail longest
    |[] -> longest

let rec tointlist words newList:list<int>=
    match words with
    |(head:string)::tail -> tointlist tail ([head.Length] @ newList)
    |[] -> newList


let rivers input =
   tointlist (tolistthing input) []
   //lengthOfthing (tolistthing input) 0    //failwith "shame"
    //longest input 0 0 0


    


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
