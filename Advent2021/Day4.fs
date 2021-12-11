module Advent2021.Day4

let calculatevalue (board: seq<(int * bool)[]>) =
    board
    |> Seq.fold (fun (acc: int) row -> row |> Array.fold (fun (inneracc:int) ((value:int), truthy) ->
            if truthy then inneracc + value else inneracc) 0) 0

let rec checkCompletedBoards boards =
    let result =
        boards
        |> Seq.take 5
        |> Seq.map (fun row -> row |> Array.filter (fun ((arrayvalue, truthy)) -> truthy))
        |> Seq.filter (fun row -> (Array.length row) > 0)
    if Seq.length result < 5 then
        calculatevalue result
    else
       if (Seq.length boards) = 5 then
           0
       else
           checkCompletedBoards (boards |> Seq.skip 5) 
    

let filterBoard board =
    board
    |> Seq.map (fun (line) -> line |> Array.filter (fun (field) -> not <| (System.String.IsNullOrWhiteSpace field)))
    |> Seq.map (fun (line) -> line |> Array.map (fun (field) -> field |> int))
    |> Seq.map (fun (line) -> line |> Array.map (fun (field) -> (field, true)))
                
let rec buildBoards currentBoards (input: seq<string>) =
    if (Seq.length input) > 4 then
        let board =
            input
            |> Seq.take 5
            |> Seq.map (fun (line) -> line.Split())
            |> Seq.toList
        buildBoards (Seq.append currentBoards (board |> filterBoard)) (Seq.skip 5 input)
    else
        currentBoards
        
let rec updateBoard (currentBoards: seq<(int * bool) []>) (input: seq<int>) =
    let result =
        currentBoards
        |> Seq.map
               (fun (value) -> value |> Array.map (fun ((arrayvalue, truthy)) ->
            if (not truthy) || arrayvalue = (Seq.head input) then (arrayvalue, false) else (arrayvalue, true)))
    let outputvalue = checkCompletedBoards result
    if outputvalue = 0 then
        if (Seq.length input) = 1 then 0 else updateBoard result (Seq.tail input)
    else
        outputvalue

let RunDay4 () =
        let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay4Input.txt")
        let inputs =
            lines
            |> Seq.head
            |> (fun (line) -> line.Split(','))
            |> Seq.map (fun (num) -> num |> int)
        let mutable boards =
            lines
            |> Seq.tail
            |> Seq.filter (fun (value) -> not (System.String.IsNullOrWhiteSpace value))
            |> buildBoards Seq.empty
            
        let results = updateBoard boards inputs
        boards
            