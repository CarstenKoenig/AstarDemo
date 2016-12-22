namespace Astar

type Score = int

// poor-mans priority queue
// via Sets
type Priority<'node when 'node : comparison> =
    { 
        nMap : Map<Score, Set<'node>>
        pMap : Map<'node, Score>
    }

module Priority =

    let empty()  : Priority<'node> =
        {
            nMap = Map.empty
            pMap = Map.empty
        }

    let mininmum (pq : Priority<'node>) =
        pq.nMap
        |> Map.pick (fun _ ns -> Some (Seq.head ns))

    let insert (n : 'node) (p : Score) (pq : Priority<'node>) =
        let nMap' =
            let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
            let lp' = Set.add n lp
            Map.add p lp' pq.nMap
        let pMap' =
            Map.add n p pq.pMap
        { nMap = nMap'; pMap = pMap' }

    let remove (n : 'node) (pq : Priority<'node>) =
        match pq.pMap.TryFind n with
        | None -> pq
        | Some p ->
            let nMap' =
                let lp = defaultArg (Map.tryFind p pq.nMap) Set.empty
                let lp' = Set.remove n lp
                if Set.isEmpty lp' then
                    Map.remove p pq.nMap
                else
                    Map.add p lp' pq.nMap
            let pMap' = Map.remove n pq.pMap
            { nMap = nMap'; pMap = pMap' }
                    