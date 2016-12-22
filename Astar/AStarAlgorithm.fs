namespace Astar

type Path<'node> = 'node seq

module Algorithm =

    type Config<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            distance : 'node -> 'node -> Score
            isGoal : 'node -> bool
        }

    type private Runtime<'node when 'node : comparison> =
        {
            heuristic : 'node -> Score
            neighbours : 'node -> 'node seq
            isGoal : 'node -> bool
            distance : 'node -> 'node -> Score
            visitedNodes : Set<'node>
            openNodes : Priority<'node>
            gScores : Map<'node,Score>
            fScores : Map<'node,Score>
            cameFrom : Map<'node,'node>
        }
        member this.GScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue
        member this.FScore node =
            defaultArg (this.gScores.TryFind node) System.Int32.MaxValue


    let private initRuntime (start : 'node) (config : Config<'node>) =
        {
            heuristic = config.heuristic
            neighbours = config.neighbours
            isGoal = config.isGoal
            distance = config.distance
            visitedNodes = Set.empty
            openNodes = Priority.empty() |> Priority.insert start 0
            gScores = Map.empty |> Map.add start 0
            fScores = Map.empty |> Map.add start (config.heuristic start)
            cameFrom = Map.empty
        }


    let rec private reconstructPath' (acc : 'node list) (toNode : 'node) (runtime : Runtime<'node>) =
        match runtime.cameFrom.TryFind toNode with
        | None -> toNode :: acc
        | Some parent -> reconstructPath' (toNode :: acc) parent runtime


    let private reconstructPath (toNode : 'node) (runtime : Runtime<'node>) =
        reconstructPath' [] toNode runtime |> Seq.ofList


    let private processChild (node : 'node) (runtime : Runtime<'node>) (child : 'node)=
        let tentativeGScore = runtime.GScore node + runtime.distance node child
        let fScoreChild = tentativeGScore + runtime.heuristic child
        let open' = runtime.openNodes |> Priority.insert child fScoreChild
        let gScoreChild = runtime.GScore child
        if tentativeGScore >= gScoreChild then
            { runtime with openNodes = open' }
        else
            { runtime with
                openNodes = open'
                cameFrom = runtime.cameFrom.Add (child, node)
                gScores = runtime.gScores.Add (child, tentativeGScore)
                fScores = runtime.fScores.Add (child, fScoreChild)
            }


    let rec private runAlgorithm (runtime : Runtime<'node>) =
        let current = runtime.openNodes |> Priority.mininmum
        if runtime.isGoal current then
            runtime |> reconstructPath current
        else
            let open' = runtime.openNodes |> Priority.remove current
            let visited' = runtime.visitedNodes |> Set.add current
            let runtime' = { runtime with openNodes = open'; visitedNodes = visited' }
            let children =
                runtime.neighbours current
                |> Seq.filter (visited'.Contains >> not)
            let runtime'' =
                children
                |> Seq.fold (processChild current) runtime'
            runAlgorithm runtime''


    let aStar (start : 'node) (config : Config<'node>) =
        config
        |> initRuntime start
        |> runAlgorithm