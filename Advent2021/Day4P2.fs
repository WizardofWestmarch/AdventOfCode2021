module Advent2021.Day4P2

let rec winBoard (board: seq<int[]>) transboard input =
    let newBoard =
        board
        |> Seq.map (fun arr -> arr |> Array.filter (input |> Seq.head))
    let newTransposeBoard =
        transboard
        |> Seq.map (fun arr -> arr |> Array.filter (input |> Seq.head))
    if newBoard |> Seq.filter (fun arr -> arr |> Array.length > 0) |> Seq.length < 5 then
        newBoard
        |> Seq.map (fun arr -> arr |> Array.sum)
    else if transboard |> Seq.filter (fun arr -> arr |> Array.length > 0) |> Seq.length < 5 then
        transboard
        |> Seq.map (fun arr -> arr |> Array.sum)
    else
        winBoard newBoard newTransposeBoard (Seq.tail input)
        
    
    

let computeScore boards input =
    boards
    |> Seq.map (fun board -> winBoard board (board |> Array.transpose) input)

let rec buildBoards currentBoards (input: seq<string>) =
    if (Seq.length input) > 4 then
        let board =
            input
            |> Seq.take 5
            |> Seq.map (fun (line) -> line.Split())
            |> Seq.map (fun (line) -> line |> Array.filter (fun (field) -> not <| (System.String.IsNullOrWhiteSpace field)))
            |> Seq.map (fun (arr) -> arr |> Array.map int)
            |> Seq.toList
        buildBoards (Seq.append currentBoards board) (Seq.skip 5 input)
    else
        currentBoards
        
let RunDay4 () =
        printfn "TEST!"
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
        //let scores = computeScore boards inputs
        ()
        