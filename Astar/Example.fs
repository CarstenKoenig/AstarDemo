namespace Astar.Example

open Astar
open System

module Maze =

    let private bits n =
        let gen n =
            if n <= 0 then None else
            Some (n % 2, n / 2)
        Seq.unfold gen n

    let private oneBits n =
        bits n
        |> Seq.filter ((<>) 0)
        |> Seq.length

    let isWall favNumber (x,y) =
        let nr = x*x + 3*x + 2*x*y + y + y*y + favNumber
        oneBits nr % 2 = 1

    let print favNumber (width, height) path =
        let onPath (x,y) =
            Seq.contains (x,y) path
        let out (x,y) =
            let onP = onPath (x,y)
            let onW = isWall favNumber (x,y)
            match (onW, onP) with
            | (true, true) -> 
                Console.ForegroundColor <- ConsoleColor.Red
                Console.Write "X"
            | (true, false) -> 
                Console.ForegroundColor <- ConsoleColor.DarkBlue
                Console.Write "#"
            | (false, true) ->
                Console.ForegroundColor <- ConsoleColor.Green
                Console.Write "O"
            | (false, false) ->
                Console.ForegroundColor <- ConsoleColor.DarkGray
                Console.Write "."
        let lineOut y =
            [0..width-1]
            |> Seq.map (fun x -> (x,y))
            |> Seq.iter out
            Console.WriteLine ""
        [0..height-1]
        |> Seq.iter lineOut


    let private dist (x,y) (x',y') =
        abs (x'-x) + abs (y'-y)

    
    let private validCoord (x,y) =
        x >= 0 && y >= 0

    let private neighbours favNumber (x,y) =
        [ (x-1,y); (x,y-1); (x+1,y); (x,y+1) ]
        |> Seq.filter (fun pos -> validCoord pos && not (isWall favNumber pos))

    let findPath favNumber fromPos toPos =
        let config : Algorithm.Config<_> =
            {
                heuristic = fun pos -> dist pos toPos
                neighbours = neighbours favNumber
                distance = fun _ _ -> 1
                isGoal = fun pos -> pos = toPos
            }
        in config |> Algorithm.aStar fromPos


module Main =

    let runProblem() =
        let favNumber = 1362
        let p = Maze.findPath favNumber (1,1) (31,39)
        Maze.print favNumber (50,50) p
        printfn "it took %d steps" (Seq.length p - 1)

    [<EntryPoint>]
    let main argv =
        runProblem()
        0 // return an integer exit code
