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

let rec recurseOrbits (o: SpaceObject ref) (orbits: SpaceObject ref list) =
    match (!o).parent with
    | Some m -> recurseOrbits m (m :: orbits)
    | None -> orbits

let indirectOrbits (o: SpaceObject ref) (orbits: SpaceObject ref list) =
    match (!o).parent with
    | Some x -> recurseOrbits x orbits
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

let readInput file =
    File.ReadAllLines file
    |> Array.fold (fun s x -> parseInput x s) getInitialSpace

[<EntryPoint>]
let main argv =
    let parsed = readInput "input_puzzle"
    let (numberOfOrbits, numberOfDirectOrbits) = getOrbitsFromSpace parsed
    printfn "Direct orbits: %d,\nIndirect orbits: %d,\nTotal orbits: %d" numberOfDirectOrbits numberOfOrbits (numberOfOrbits + numberOfDirectOrbits)
    0 // return an integer exit code
