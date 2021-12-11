module Advent2021.Day3
//length per line is 12
//NOTE: I need to recalculate the position information for characters after each filtration round as part of part 2.
let mutable positionOneCounts = [|0;0;0;0;0;0;0;0;0;0;0;0|]
let mutable positionZeroCounts = [|0;0;0;0;0;0;0;0;0;0;0;0|]

let rec parseline (lines: List<string>) =
    match lines with
    | [head] ->
            for i in 0 .. 11 do
                match head[i] with
                | '1' -> positionOneCounts[i] <- positionOneCounts[i] + 1
                | _ -> positionZeroCounts[i] <- positionZeroCounts[i] + 1
    | head :: rest ->
        for i in 0 .. 11 do
            match head[i] with
            | '1' -> positionOneCounts[i] <- positionOneCounts[i] + 1
            | _ -> positionZeroCounts[i] <- positionZeroCounts[i] + 1
        parseline rest
     | [] -> ()
//I should go back and change the > line to a lamdba that takes the two values so I can pass in if I want > or < "configurably" to manage
//both O2 and CO2
let rec filterLines (position: int) (lines: List<string>) =
    let output =
        lines
        |> List.filter (fun (line) ->
            if positionOneCounts[position] > positionZeroCounts[position] then
                line[position] = '1'
            else
                line[position] = '0')
    if position < 10 then
        filterLines (position + 1) output
    else output

//I was lazy here, this is calculating just the O2. Have to swap the > and < and swap out which wins when they are even. I simply ran the code twice after
//swapping the values because getting both > and < and the default value was more effort then I wanted to put in right now. Technically could just make two copies of the
//function but I'm a lazy person for Advent of Code.
let rec newFilterLines (position: int) (lines: List<string>) =
    let oneOutput =
        lines
        |> List.filter (fun (line) ->
            line[position] = '1')
    let zeroOutput =
        lines
        |> List.filter (fun (line) ->
            line[position] = '0')
    let newoutput =
        match zeroOutput with
        | _ when (List.length zeroOutput) > (List.length oneOutput) -> zeroOutput
        | _ when (List.length zeroOutput) < (List.length oneOutput) -> oneOutput
        | _ -> oneOutput
    if position < 11 && (List.length newoutput) > 1 then
        newFilterLines (position + 1) newoutput
    else
        newoutput
        
let RunDay3 () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay3Input.txt")
    let lenvalue = lines |> Seq.length
    parseline (Seq.toList lines)
    printfn "Total lines: %i" lenvalue
    printfn "gamma value"
    for i in 0..11 do
        if positionOneCounts[i] > positionZeroCounts[i] then printf "1" else printf "0"
    printfn "epsilon value"
    for i in 0..11 do
        if positionOneCounts[i] < positionZeroCounts[i] then printf "1" else printf "0"
    for i in 0..11 do
        printfn "Position '%i' One Count: %i Zero Count: %i" i positionOneCounts[i] positionZeroCounts[i]
        
    
    let outputvalue = newFilterLines 0 (Seq.toList lines)
    for i in 0..11 do
        positionOneCounts[i] <- 0
        positionZeroCounts[i] <- 0
    parseline outputvalue
    printfn "Oxygen value"
    for i in 0..11 do
        if positionOneCounts[i] > positionZeroCounts[i] then printf "1" else printf "0"
    printfn " "
    printfn "CO2 value"
    for i in 0..11 do
        if positionOneCounts[i] < positionZeroCounts[i] then printf "1" else printf "0"
    printfn " "
    ()
     
    