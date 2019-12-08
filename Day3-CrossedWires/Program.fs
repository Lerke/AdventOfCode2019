open System.Text.RegularExpressions

type Direction =
    | Up = 'U'
    | Right = 'R'
    | Left = 'L'
    | Down = 'D'
    | Reset = 'Q'

type Tile =
    | Empty = '.'
    | Corner = '+'
    | Vertical = '|'
    | Horizontal = '-'
    | Intersection = 'X'
    | Start = 'O'
    
type Position = int * int
type Circuit = Map<Position, (Tile * int * int) list> * Position
type Move = Direction * int * int

// Given some regular expression, match it with some input
let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

// Update some position in the circuit with a new tile value.
// If a tile is already present, it will be marked as an intersection instead.
let updateCircuitLocation (world:Circuit) (position: Position) (tile: (Tile * int * int)) =
    let (tiles, pos) = world
    let (tileValue, tileWire, tileCount) = tile
    let newTiles = match tiles.TryFind position with
                    | Some existing when List.forall (fun (_, wire, _) -> wire <> tileWire) existing -> if List.exists (fun (tile, _, _) -> tile = Tile.Start) existing then tiles else tiles.Add(position, List.append existing [ ( Tile.Intersection, tileWire, tileCount) ])
                    | Some existing when List.exists (fun (_, wire, _) -> wire = tileWire) existing -> if List.exists (fun (tile, _, _) -> tile = Tile.Start) existing then tiles else tiles
                    | None _ -> tiles.Add(position, [ (tileValue, tileWire, tileCount) ])
    (newTiles, pos)

// Perform a movement in a circuit.
let moveInCircuit (circuit: Circuit) (direction: Direction) =
    let (tiles, position) = circuit
    let (x,y) = position
    match direction with
    | Direction.Up -> (tiles, (x, y+1))
    | Direction.Down -> (tiles, (x, y-1))
    | Direction.Left -> (tiles, (x-1, y))
    | Direction.Right -> (tiles, (x+1, y))
    | Direction.Reset -> (tiles, (0,0))

// Retrieve all tiles marked as intersections in a circuit
let getIntersections (circuit: Circuit) =
    Map.filter (fun key value -> List.exists (fun (tile, _, _) -> tile = Tile.Intersection) value) (fst circuit)

// Calculate the manhattan distance between two positions
// This is calculated via |(x2 - x1)| + |(y2 - y1)|
let calculateManhattanDistance (position: Position) (origin: Position) =
    let (xPosition, yPosition) = position
    let (xOrigin, yOrigin) = origin
    abs (xOrigin - xPosition) + abs (yOrigin - yPosition)

let resetPosition (circuit:Circuit) : Circuit =
    let (tiles, _) = circuit
    (tiles, (0,0))
    
// Parse individual moves as string to movements.
let calculateMove (input: string) (wire: int) =
    match input with
    | Regex @"U(\d+)" [ moves ] -> Direction.Up, int moves, wire
    | Regex @"D(\d+)" [ moves ] -> Direction.Down, int moves, wire
    | Regex @"L(\d+)" [ moves ] -> Direction.Left, int moves, wire
    | Regex @"R(\d+)" [ moves ] -> Direction.Right, int moves, wire
    | _ -> failwith "Unsupported move type"

// Given a circuit and a move, calculate the next state of the circuit.
let rec performMove (circuit: Circuit) (move: Move) (count: int) : (Circuit * int) =
    let (_, position) = circuit
    let (xPos, yPos) = position
    match move with
    | Direction.Up, x, wire when x > 0 ->
        let newWorld = updateCircuitLocation circuit (xPos, yPos + 1) (Tile.Vertical, wire, count + 1)
        let returnWorld = ((performMove (moveInCircuit newWorld Direction.Up) (Direction.Up, x - 1, wire) (count + 1)))
        (fst returnWorld, snd returnWorld)
    | Direction.Down, x, wire when x > 0 ->
        let newWorld = updateCircuitLocation circuit (xPos, yPos - 1) (Tile.Vertical, wire, count + 1)
        let returnWorld = ((performMove (moveInCircuit newWorld Direction.Down) (Direction.Down, x - 1, wire) (count + 1)))
        (fst returnWorld, snd returnWorld)
    | Direction.Left, x, wire when x > 0 ->
        let newWorld = updateCircuitLocation circuit (xPos - 1, yPos) (Tile.Horizontal, wire, count + 1)
        let returnWorld = ((performMove (moveInCircuit newWorld Direction.Left) (Direction.Left, x - 1, wire) (count + 1)))
        (fst returnWorld, snd returnWorld)
    | Direction.Right, x, wire when x > 0 ->
        let newWorld = updateCircuitLocation circuit (xPos + 1, yPos) (Tile.Horizontal, wire, count + 1)
        let returnWorld = ((performMove (moveInCircuit newWorld Direction.Right) (Direction.Right, x - 1, wire) (count + 1)))
        (fst returnWorld, snd returnWorld)
    | Direction.Reset, _, _ -> (resetPosition circuit, 0)
    | _, _, _ -> (circuit, count)

// Take a list of input lines and convert them to a list of moves.
let parseInput (input: string[]) =
    input
    |> Array.mapi (fun x value -> (x, value))
    |> Array.fold (fun x (index, y:string) -> Array.concat [
                                            x // Use the previously calculated set of moves
                                            (Array.map(fun m -> calculateMove m index) (y.Split [| ',' |]) ) // Split by ',' and parse moves
                                            [| (Direction.Reset, 0, index) |] // Append reset move after this wire.
                                          ]) [|  |] // Start with empty initial list of moves.
    
// Read input from the input file and parse it as a list of moves.
let readInput input =
    System.IO.File.ReadAllLines(input)
    |> parseInput

// Given a set of moves, calculate the final state of a circuit
let calculateCircuit moves =
    // Define an initial circuit and calculate.
    let circuit: Circuit = Map.ofList [ ((0, 0) , [ (Tile.Start, 0, 0) ]) ], (0, 0)
    Array.foldi (fun index newCircuit move -> performMove (fst newCircuit) move (snd newCircuit)) (circuit, 0) moves

// Given a set of points, calculate the manhattan distance for each one of them.
let retrieveManhattanDistances (intersections:Map<Position,(Tile * int * int) list>) : (Position * int) list =
    intersections
    |> Map.map (fun key _ -> calculateManhattanDistance key (0,0))
    |> Map.toList

let retrieveMinimumSignalDistances (circuit: Circuit) (intersections:Map<Position,(Tile * int * int) list>) : (Position * int) list =
    let (tiles, _) = circuit
    intersections
    |> Map.map (fun key _ -> Map.fold (fun acc key value -> acc + List.fold (fun acc (_,_, distance) -> acc + distance) 0 value) 0 (Map.filter (fun k _ -> k = key) tiles))
    |> Map.toList

// Given a circuit, calculate the minimum manhattan distance from the origin
// To any intersection.
let calculateDistance (circuit: Circuit) distanceFunction =
    let intersections = getIntersections (circuit)
    let distances =
        distanceFunction intersections
        |> List.sortBy (fun (_, value) -> value)
    distances.Head

[<EntryPoint>]
let main argv =
    let input = readInput "input"
    let circuit = calculateCircuit input
    let ((xPos, yPos), distance) = (calculateDistance (fst circuit) retrieveManhattanDistances)
    printfn "Closest distance: (%d, %d): %d" xPos yPos distance
    
    let signalDistance = retrieveMinimumSignalDistances (fst circuit)
    let ((xPos2, yPos2), distance2) = (calculateDistance (fst circuit) signalDistance)
    printfn "Closest signal distance: (%d, %d): %d" xPos2 yPos2 distance2
    0 // return an integer exit code
