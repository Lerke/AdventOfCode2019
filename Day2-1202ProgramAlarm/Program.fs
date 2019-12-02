/// Possible operation types of our simple computer
type OperationType = | Add = 1 | Multiply = 2 | Stop = 99

/// Transform an input number into one of the possible opcodes
let getOperation num =
    match num with
    | 1 -> OperationType.Add
    | 2 -> OperationType.Multiply
    | 99 -> OperationType.Stop

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
    printfn "%A" result
    match result with
    | (noun,verb) ->
        printfn "Answer Puzzle Two: (%d, %d) => 100 * %d + %d = %d" noun verb noun verb (100 * noun + verb)
    

[<EntryPoint>]
let main argv =
    let input = [ 1;0;0;3;1;1;2;3;1;3;4;3;1;5;0;3;2;1;13;19;1;9;19;23;2;13;23;27;2;27;13;31;2;31;10;35;1;6;35;39;1;5;39;43;1;10;43;47;1;5;47;51;1;13;51;55;2;55;9;59;1;6;59;63;1;13;63;67;1;6;67;71;1;71;10;75;2;13;75;79;1;5;79;83;2;83;6;87;1;6;87;91;1;91;13;95;1;95;13;99;2;99;13;103;1;103;5;107;2;107;10;111;1;5;111;115;1;2;115;119;1;119;6;0;99;2;0;14;0 ];
    puzzleOne input
    puzzleTwo input 19690720
    0
