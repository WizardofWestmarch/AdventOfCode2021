module Advent2021.Day6

let rec updateFish totalDays currentDay schoolOfFish =
    let newFish =
        schoolOfFish
        |> List.filter (fun f -> f = 0)
        |> List.map (fun _ -> 8)
    let updatedNewSchool =
        schoolOfFish
        |> List.map (fun fish -> if fish = 0 then 7 else fish)
    let newSchool =
        updatedNewSchool
        |> List.map (fun fish -> fish - 1)
    
    if currentDay < totalDays then
        updateFish totalDays (currentDay + 1) (newSchool @ newFish)
    else
        (updatedNewSchool @ newFish) |> Seq.length
let rec updateFishNew totalDays currentDay (schoolOfFish: seq<int*int64>) =
    let newFish =
        schoolOfFish
        |> Seq.filter (fun (age, _) -> age = 0)
        |> Seq.map (fun (_, count) ->  count)
        |> Seq.sum
    let updatedSchool =
        schoolOfFish
        |> Seq.filter(fun (age, _) -> age > 0)
        |> Seq.map(fun (age, count) -> if age = 7 then (age, count + newFish) else (age, count))
        |> Seq.map(fun (age, count) -> (age - 1, count))
    if totalDays > currentDay then
        
        updateFishNew totalDays (currentDay + 1) (updatedSchool |> Seq.append [(8, newFish)])
        
    else
        newFish + (updatedSchool |> Seq.map (fun (_, count) -> count) |> Seq.sum)
    
    
        
    

let RunDay () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay6Input.txt")
    let school =
        lines
        |> Seq.map (fun line -> line.Split(','))
        |> Seq.map (fun line -> line |> Array.map int)
        |> Seq.concat
        |> List.ofSeq
    let (testSchool: seq<int*int64>) =
        school
        |> Seq.groupBy id
        |> Seq.map (fun (count, groupedFish) -> (count, int64((groupedFish |> Seq.length))))
        |> Seq.append [(8,(int64 "0")); (7,(int64 "0"))]
    
    let result = updateFish 80 1 school
    let result2 = updateFishNew 256 1 testSchool
    (*let result0 = result |> List.filter (fun f -> f = 0) |> List.length
    let result1 = result |> List.filter (fun f -> f = 1) |> List.length
    let result2 = result |> List.filter (fun f -> f = 2) |> List.length
    let result3 = result |> List.filter (fun f -> f = 3)|> List.length
    let result4 = result |> List.filter (fun f -> f = 4)|> List.length
    let result5 = result |> List.filter (fun f -> f = 5)|> List.length
    let result6 = result |> List.filter (fun f -> f = 6)|> List.length
    let result7 = result |> List.filter (fun f -> f = 7)|> List.length
    let result8 = result |> List.filter (fun f -> f = 8)|> List.length*)
    printfn "Old Result: %i  :: New Result: %i" result result2
    ()