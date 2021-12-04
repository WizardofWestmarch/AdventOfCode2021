module Advent2021.Day2

let mutable currenthorizontal: int = 0
let mutable currentdepth: int = 0
let mutable aim: int = 0
let parselinePart2 (input:string) =
    
    let values = input.Split(" ") |> Array.toList
    match values with
    | "forward" :: [x] ->
        currenthorizontal <- currenthorizontal + (int x)
        currentdepth <- currentdepth + (aim * (int x))
    | "up" :: [x] -> aim <- aim - (int x)
    | "down" :: [x] -> aim<- aim + (int x)
    | _ -> printfn "Hit a bad case"

let parselinePart1 (input:string) =
    let values = input.Split(" ") |> Array.toList
    match values with
    | "forward" :: [x] -> currenthorizontal <- currenthorizontal + (int x)
    | "up" :: [x] -> currentdepth <- currentdepth - (int x)
    | "down" :: [x] -> currentdepth<- currentdepth + (int x)
    | _ -> printfn "Hit a bad case"
    


let RunDay2 () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay2Input.txt")
    lines
    |> Seq.iter parselinePart1
    printfn "Day 2 Part 1 Horizontal: %i  Depth: %i" currenthorizontal currentdepth
    currentdepth <- 0
    currenthorizontal <- 0
    lines
    |> Seq.iter parselinePart2
    printfn "Day 2 Part 1 Horizontal: %i  Depth: %i  Aim: %i" currenthorizontal currentdepth aim
    