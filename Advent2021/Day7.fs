
module Advent2021.Day7
open System

//on the (fun i -> [1..i] |> List.sum replace with just nothing or (fun i -> i)
let calculateCost position (arr: int[]) =
    arr
    |> Array.map (fun i -> (i - position) |> Math.Abs |> (fun i -> [1..i] |> List.sum))
    |> Array.sum
    

let RunDay () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay7Input.txt")
    let crabs =
        lines
        |> Seq.map (fun line -> line.Split(','))
        |> Seq.map (fun line -> line |> Array.map (fun number -> number |> int))
        |> Seq.head
    let totalvalue =
        crabs
        |> Seq.sum
    let sortedValues = crabs |> Array.sort
    let maxValue = sortedValues |> Array.last
    
    let minCost =
        [0..maxValue]
        |> List.fold (fun lowestcost value ->
            let cost = calculateCost value crabs
            if cost < lowestcost then cost else lowestcost) Int32.MaxValue
    
        
    
    ()