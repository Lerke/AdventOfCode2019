open System
open System

/// Possible operation types of our simple computer
type OperationType =
    | Add = 1
    | Multiply = 2
    | Store = 3
    | Output = 4
    | Stop = 99

/// Transform an input number into one of the possible opcodes
let getOperation num =
    match num with
    | 1 -> OperationType.Add
    | 2 -> OperationType.Multiply
    | 3 -> OperationType.Store
    | 4 -> OperationType.Output
    | 99 -> OperationType.Stop

let rec readInput() =
    printf "Input: "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, i -> i
    | false, _ ->
        printfn "Try again"
        readInput()

let output value =
    printfn "OUTPUT: %d" value

/// Update the memory at index. Returns a new collection where index has been replaced with value.
let updateMemoryLocation memory index value = List.mapi (fun i x -> if i = index then value else x ) memory

/// Calculate the next memory layout of the computer.
let rec calculateNext (memory:List<int>) ip =
    let operation = memory.Item(ip)
    match getOperation operation with
    | OperationType.Add ->
         calculateNext (updateMemoryLocation
                            memory
                            (memory.Item(ip+3)) ((memory.Item(memory.Item(ip+1)) + (memory.Item(memory.Item(ip+2)))))) (ip + 4)
    | OperationType.Multiply ->
         calculateNext (updateMemoryLocation
                            memory
                            (memory.Item(ip+3)) ((memory.Item(memory.Item(ip+1)) * (memory.Item(memory.Item(ip+2)))))) (ip + 4)
    | OperationType.Store ->
        let input = readInput()
        calculateNext (updateMemoryLocation
                            memory
                            (memory.Item(ip+1)) (input)) (ip + 2)
    | OperationType.Output ->
        output (memory.Item(memory.Item(ip+1)))
        calculateNext memory (ip + 2)
    | OperationType.Stop -> memory
    
/// Start calculating until halt given a certain memory map. Assume Instruction Pointer starts at 0
let doCalculation (memory:List<int>) =
    calculateNext memory 0
    
/// Solve the first puzzle
let puzzleOne input =
    printfn "Output Puzzle One %A" (doCalculation (updateMemoryLocation(updateMemoryLocation input 1 12) 2 2))
    printfn "Answer Puzzle One %d" ((doCalculation (updateMemoryLocation(updateMemoryLocation input 1 12 ) 2 2)).Item(0))
    
/// Solve the second puzzle
let puzzleTwo input target =
    let searchSpace = [1 .. 99] |> List.collect(fun x -> [1 .. 99] |> List.map(fun y -> x, y))
    let result = searchSpace
                 |> List.find (fun (noun,verb) ->
                     (doCalculation
                          ((updateMemoryLocation
                                (updateMemoryLocation input 1 noun)
                                2 verb))).Item(0) = target)
    match result with
    | (noun,verb) ->
        printfn "Answer Puzzle Two: (%d, %d) => 100 * %d + %d = %d" noun verb noun verb (100 * noun + verb)
    

[<EntryPoint>]
let main argv =
    let input = [ 3;0;4;0;99 ]
    let result = doCalculation input
//    puzzleOne input
//    puzzleTwo input 19690720
    0
