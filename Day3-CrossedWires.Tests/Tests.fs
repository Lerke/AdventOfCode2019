module Tests

open Program
open FsUnit
open Xunit

[<Fact>]
let ``Example One Manhattan Distance`` () =
    let input = [| "R8,U5,L5,D3" ; "U7,R6,D4,L4" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) retrieveManhattanDistances
    distance |> should equal 6

[<Fact>]
let ``Example Two Manhattan Distance`` () =
    let input = [| "R75,D30,R83,U83,L12,D49,R71,U7,L72" ; "U62,R66,U55,R34,D71,R55,D58,R83" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) retrieveManhattanDistances
    distance |> should equal 159

[<Fact>]
let ``Example Three Manhattan Distance`` () =
    let input = [| "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) retrieveManhattanDistances
    distance |> should equal 135

[<Fact>]
let ``Example One Signal Distance`` () =
    let input = [| "R8,U5,L5,D3" ; "U7,R6,D4,L4" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let signalFunction = retrieveMinimumSignalDistances (fst circuit)
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) signalFunction
    distance |> should equal 30
    Assert.True(true)

[<Fact>]
let ``Example Two Signal Distance`` () =
    let input = [| "R75,D30,R83,U83,L12,D49,R71,U7,L72" ; "U62,R66,U55,R34,D71,R55,D58,R83" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let signalFunction = retrieveMinimumSignalDistances (fst circuit)
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) signalFunction
    distance |> should equal 610

[<Fact>]
let ``Example Three Signal Distance`` () =
    let input = [| "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"; "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" |]
    let moves = parseInput input
    let circuit = calculateCircuit moves
    let signalFunction = retrieveMinimumSignalDistances (fst circuit)
    let ((xPos, yPos), distance) = calculateDistance (fst circuit) signalFunction
    distance |> should equal 410
