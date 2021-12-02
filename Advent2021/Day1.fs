module Advent2021.Day1

let rec foldfunc (currentlist: List<string>) lastvalue counter =
    match currentlist with
    | head :: rest when int head > lastvalue -> foldfunc rest (int head) (counter+1)
    | head :: rest -> foldfunc rest (int head) counter
    | [ head ] when (int head) > lastvalue -> counter + 1
    | [ _ ] -> counter
    | _ -> counter


//NOTE: I start at -1 because the first pass always adds as I am lazy.
let rec foldfunc2 (currentlist: List<string>) lastsum counter =
    match currentlist with
    | head1 :: head2 :: head3 :: rest when (int head1) + (int head2) + (int head3) > lastsum -> foldfunc2 currentlist.Tail ((int head1) + (int head2) + (int head3)) (counter + 1)
    | head1 :: head2 :: head3 :: rest -> foldfunc2 currentlist.Tail ((int head1) + (int head2) + (int head3)) counter
    | head1 :: head2 :: [head3] when (int head1) + (int head2) + (int head3) > lastsum -> counter
    | _ :: _ :: [_] -> counter
    | _ -> counter
    
let foldfunc3 currentlist =
    currentlist
    |> Seq.map int
    |> Seq.windowed 4
    |> Seq.filter (fun (values) -> (values.[0] + values.[1] + values.[2]) < (values.[1] + values.[2] + values.[3]))
    |> Seq.length
    
let foldfunc4 currentlist windowsize =
    currentlist
    |> Seq.map int
    |> Seq.windowed windowsize
    |> Seq.filter (fun (values) -> values |> (fun (newvalues) -> (Seq.sum newvalues) - newvalues.[windowsize - 1] < (Seq.sum newvalues) - newvalues.[0]))
    |> Seq.length

let RunDay1 () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay1Input.txt")
    let result = foldfunc (Seq.toList lines) 0 -1
    printfn "Day 1 Part 1: %i" result
    let result2 = foldfunc2 (Seq.toList lines) 0 -1
    printfn "Day 1 Part 2: %i" result2
    let result3 = foldfunc3 lines
    printfn "Day 1 Test1 %i" result3
    let result5 = foldfunc4 lines 2
    printfn "Day 1 Part 1 experiment %i" result5
    let result4 = foldfunc4 lines 4
    printfn "Day 1 Part 2 experiment %i" result4
    