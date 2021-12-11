module Advent2021.Day5


type Point =
    { x: int
      y: int
    }

let rec myFunc points =
    points
    |> Seq.groupBy (fun x -> x)
    |> Seq.map (fun (g,i) -> g, i |> Seq.length)
    |> Seq.filter (fun (_, c) -> c > 1)
    |> Seq.map (fun (c, _) -> c)
    |> Seq.length

let generateP1Points p2value p1begin p1end =
    [p1begin..p1end]
    |> Seq.map (fun value -> {x=value; y=p2value})
let generateP2Points p1value p2begin p2end =
    [p2begin..p2end]
    |> Seq.map (fun value -> {x=p1value; y=value})

let createline (pointa:Point) (pointb:Point) =
    if pointa.x = pointb.x then
        if pointa.y < pointb.y then
            generateP2Points pointa.x pointa.y pointb.y
        else
            generateP2Points pointa.x pointb.y pointa.y
    else if pointa.y = pointb.y then
        if pointa.x < pointb.x then
            generateP1Points pointa.y pointa.x pointb.x
        else
            generateP1Points pointa.y pointb.x pointa.x
    else
        //Seq.empty
        let dirX = if pointa.x < pointb.x then 1 else -1
        let dirY = if pointa.y < pointb.y then 1 else -1
        [ pointa.y .. dirY .. pointb.y]
        |> Seq.zip[ pointa.x .. dirX .. pointb.x]
        |> Seq.map (fun (x1, y1) -> {x=x1;y=y1})

let RunDay5 () =
    let lines = System.IO.File.ReadLines(@"C:\users\dorma\Documents\AdventDay5Input.txt")
    let aline = createline {x=1;y=5} {x=10;y=5}
    let coords =
        lines
        |> Seq.map (fun line -> line.Split("->"))
        |> Seq.map (fun line -> line |> Array.map (fun values -> values.Split(',')))
        |> Seq.map (fun line -> line |> Array.map (fun values1 -> values1 |> Array.map (fun values2 -> values2 |> int)))
        |> Seq.map (fun line -> line |> Array.map (fun values1 -> {x=values1[0];y=values1[1]}))
        //|> Seq.filter (fun line -> line[0].x = line[1].x || line[0].y = line[1].y) This line gets only points for vert/horizontal to answer day 1. Remove it to get day 2.
    //let results = myFunc Set.empty coords
    let allPoints =
        coords
        |> Seq.map (fun line -> createline line[0] line[1])
        |> Seq.concat
    let results = myFunc allPoints
    ()