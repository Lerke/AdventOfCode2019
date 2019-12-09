open System.IO
open FSharp.Text.RegexProvider

type inputRegex = Regex< @"^(?<parent>[^\)]+)\)(?<child>[^\)]+)$" >

type SpaceObject = { name: string
                     parent: SpaceObject ref Option
                     children: SpaceObject ref list }
and Space = { centerOfMass: SpaceObject ref
              objects: Map<string, SpaceObject ref> }

let parseInput (line: string) (space: Space) =
    let (parent, child) =
        match inputRegex().TryTypedMatch(line) with
        | Some m -> (m.parent.Value, m.child.Value)
        | None -> failwithf "Invalid input: %s" line
    
    let parentObject =
        match space.objects.TryFind parent with
        | Some o -> o
        | None -> ref { name = parent
                        parent = None
                        children = [] }
    let childObject =
        match space.objects.TryFind child with
        | Some o -> o
        | None -> ref { name = child
                        parent = Some parentObject
                        children = [] }
        
    childObject := { !childObject with parent = Some parentObject }
    parentObject := { !parentObject with children = (childObject :: (!parentObject).children) }
    { space with objects = ((space.objects.Add (child, childObject)).Add (parent, parentObject))
                 centerOfMass = if parent = "COM" then parentObject else space.centerOfMass }

let getInitialSpace =
    { centerOfMass = ref ({ name = "COM"
                            parent = None
                            children = [] })
      objects = Map.empty }
    
let getOrbit (object: SpaceObject ref) =
    match (!object).parent with
    | Some m -> Some m
    | None -> None

let rec recurseOrbits (o: SpaceObject ref) (orbits: (SpaceObject ref * int) list) (steps: int) =
    match (!o).parent with
    | Some m -> recurseOrbits m ((m, steps) :: orbits) (steps + 1)
    | None -> orbits

let indirectOrbits (o: SpaceObject ref) (orbits: (SpaceObject ref * int) list) =
    match (!o).parent with
    | Some x -> recurseOrbits x orbits 0
    | None -> orbits

let directOrbits (o: SpaceObject ref) =
    match (!o).parent with
    | Some x -> [ x ]
    | None -> [  ]

let getOrbitsFromSpace space =
    let orbits = Map.map (fun key value -> indirectOrbits value []) space.objects
    let numberOfOrbits = Map.fold (fun acc key value -> acc + (List.length value) ) 0 orbits
    let directOrbits = Map.map(fun key value -> directOrbits value) space.objects
    let numberOfDirectOrbits = Map.fold (fun acc key value -> acc + (List.length value) ) 0 directOrbits
    (numberOfOrbits, numberOfDirectOrbits)

let traceShortestPath (startObject: SpaceObject ref) (endObject: SpaceObject ref) =
    let indirectOrbitsStart = indirectOrbits startObject []
    let indirectOrbitsEnd = indirectOrbits endObject []
    let (closestParent, _) = List.last (indirectOrbitsStart |> List.filter (fun (x, xsteps) -> List.exists (fun (y, ysteps) -> (!y).name = (!x).name) indirectOrbitsEnd))
    let shortestPath =
        List.append indirectOrbitsStart indirectOrbitsEnd
        |> List.filter (fun (x, _) -> (!x).name = (!closestParent).name)
        |> List.fold (fun acc (_, steps) -> acc + steps) 0
    shortestPath
    |> fun x -> match (!startObject).parent with
                | Some p -> if (!p).name <> (!closestParent).name then x + 1 else x
                | None -> x
    |> fun x -> match (!endObject).parent with
                | Some p -> if (!p).name <> (!closestParent).name then x + 1 else x
                | None -> x
    
    
let readInput file =
    File.ReadAllLines file
    |> Array.fold (fun s x -> parseInput x s) getInitialSpace

[<EntryPoint>]
let main argv =
    let parsed = readInput "input_puzzle"
    let (numberOfOrbits, numberOfDirectOrbits) = getOrbitsFromSpace parsed
    printfn "Direct orbits: %d,\nIndirect orbits: %d,\nTotal orbits: %d" numberOfDirectOrbits numberOfOrbits (numberOfOrbits + numberOfDirectOrbits)
    let shortestPath = traceShortestPath (parsed.objects.Item("YOU")) (parsed.objects.Item("SAN"))
    printfn "Shortest distance between YOU and SAN: %d" shortestPath
    0 // return an integer exit code
