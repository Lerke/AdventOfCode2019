open System

/// Possible operation types of our simple computer
type OperationType =
    | Add = 1
    | Multiply = 2
    | Store = 3
    | Output = 4
    | JumpIfTrue = 5
    | JumpIfFalse = 6
    | LessThan = 7
    | Equals = 8
    | Stop = 99

// Possible Parameter modes
type ParameterMode =
    | Position = 0    // Use parameter as an index into memory
    | Immediate = 1   // Use parameter as a value.

let rec getIndividualDigits (numList: int list) number =
    match number with
    | x when x > 0 -> getIndividualDigits (List.append numList [ x % 10 ]) (x / 10)
    | x ->
        List.concat [ numList; [0;0;0;0;0] ]
        |> List.take 5
        |> List.rev
    | _ -> [ ]

// Transform int to parameter node type
let getParamMode mode =
    match mode with
    | 1 -> ParameterMode.Immediate
    | _ -> ParameterMode.Position
        
/// Transform an input number into one of the possible opcodes
let getOperation num =
    let digits = num
                 |> getIndividualDigits []
    // Split up num
    let opcode = match List.last digits with
                    | 1 -> OperationType.Add
                    | 2 -> OperationType.Multiply
                    | 3 -> OperationType.Store
                    | 4 -> OperationType.Output
                    | 5 -> OperationType.JumpIfTrue
                    | 6 -> OperationType.JumpIfFalse
                    | 7 -> OperationType.LessThan
                    | 8 -> OperationType.Equals
                    | 99 -> OperationType.Stop
                    | _ -> OperationType.Stop

    let digits = num
                     |> getIndividualDigits []
    let paramModes = digits
                     |> List.rev
                     |> List.skip 2 // Skip Opcode
                     |> List.map (fun x -> match x with
                                            | 1 -> ParameterMode.Immediate
                                            | _ -> ParameterMode.Position)
    (opcode, paramModes)
    
// Reads input from console
let rec readInput() =
    printf "Input: "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, i -> i
    | false, _ ->
        printfn "Try again"
        readInput()

// Outputs a value to the screen
let output value =
    printfn "OUTPUT: %d" value

/// Update the memory at index. Returns a new collection where index has been replaced with value.
let updateMemoryLocation memory index value = List.mapi (fun i x -> if i = index then value else x ) memory

let getModes (memory: List<int>) (ip: int) (modes: ParameterMode list) num =
    let operandOne = if modes.Length >= 1 && num >= 1 then match (modes.Item(0)) with 
                                                            | ParameterMode.Immediate -> memory.Item(ip+1)
                                                            | ParameterMode.Position  ->if num = 1 then memory.Item(ip+1) else memory.Item(memory.Item(ip+1))
                                                            | _ -> 0
                                          else 0
    let operandTwo = if modes.Length >= 2 && num >= 2 then match (modes.Item(1)) with 
                                                            | ParameterMode.Immediate -> memory.Item(ip+2)
                                                            | ParameterMode.Position  -> if num = 2 then memory.Item(ip+2) else memory.Item(memory.Item(ip+2))
                                                            | _ -> 0
                                          else 0 
    let operandThree = if modes.Length >= 3 && num >= 3 then match (modes.Item(2)) with 
                                                            | ParameterMode.Immediate -> memory.Item(ip+3)
                                                            | ParameterMode.Position  -> memory.Item(ip+3) //memory.Item(memory.Item(ip+3))
                                                            | _ -> 0
                                            else 0
    (operandOne, operandTwo, operandThree)
    

/// Calculate the next memory layout of the computer.
let rec calculateNext (memory:List<int>) ip =
    let operation = memory.Item(ip)
    match getOperation operation with
    | (OperationType.Add, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext (updateMemoryLocation memory (o3) ((o1 + o2))) (ip + 4)
    | (OperationType.Multiply, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext (updateMemoryLocation
                            memory
                            (o3) ((o1 * o2))) (ip + 4)
    | (OperationType.Store, modes) ->
        let input = readInput()
        let (o1, o2, o3) = getModes memory ip modes 1
        calculateNext (updateMemoryLocation
                            memory
                            (memory.Item(ip+1)) (input)) (ip + 2)
    | (OperationType.Output, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 2
        output (o1)
        calculateNext memory (ip + 2)
    | (OperationType.JumpIfTrue, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let newIp = if o1 <> 0 then o2 else (ip + 3)
        calculateNext memory newIp
    | (OperationType.JumpIfFalse, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let newIp = if o1 = 0 then o2 else (ip + 3)
        calculateNext memory newIp
    | (OperationType.LessThan, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let result = if o1 < o2 then 1 else 0
        calculateNext (updateMemoryLocation
                            memory
                            (o3) (result)) (ip + 4)
    | (OperationType.Equals, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let result = if o1 = o2 then 1 else 0
        calculateNext (updateMemoryLocation
                            memory
                            (o3) (result)) (ip + 4)
    | (OperationType.Stop, _) -> memory

/// Start calculating until halt given a certain memory map. Assume Instruction Pointer starts at 0
let doCalculation (memory:List<int>) =
    calculateNext memory 0

[<EntryPoint>]
let main argv =
    let input = Array.toList [| 3; 225; 1; 225; 6; 6; 1100; 1; 238; 225; 104; 0; 2; 106; 196; 224; 101; -1157; 224; 224; 4; 224; 102; 8; 223; 223; 1001; 224; 7; 224; 1; 224; 223; 223; 1002; 144; 30; 224; 1001; 224; -1710; 224; 4; 224; 1002; 223; 8; 223; 101; 1; 224; 224; 1; 224; 223; 223; 101; 82; 109; 224; 1001; 224; -111; 224; 4; 224; 102; 8; 223; 223; 1001; 224; 4; 224; 1; 223; 224; 223; 1102; 10; 50; 225; 1102; 48; 24; 224; 1001; 224; -1152; 224; 4; 224; 1002; 223; 8; 223; 101; 5; 224; 224; 1; 223; 224; 223; 1102; 44; 89; 225; 1101; 29; 74; 225; 1101; 13; 59; 225; 1101; 49; 60; 225; 1101; 89; 71; 224; 1001; 224; -160; 224; 4; 224; 1002; 223; 8; 223; 1001; 224; 6; 224; 1; 223; 224; 223; 1101; 27; 57; 225; 102; 23; 114; 224; 1001; 224; -1357; 224; 4; 224; 102; 8; 223; 223; 101; 5; 224; 224; 1; 224; 223; 223; 1001; 192; 49; 224; 1001; 224; -121; 224; 4; 224; 1002; 223; 8; 223; 101; 3; 224; 224; 1; 223; 224; 223; 1102; 81; 72; 225; 1102; 12; 13; 225; 1; 80; 118; 224; 1001; 224; -110; 224; 4; 224; 102; 8; 223; 223; 101; 2; 224; 224; 1; 224; 223; 223; 4; 223; 99; 0; 0; 0; 677; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1105; 0; 99999; 1105; 227; 247; 1105; 1; 99999; 1005; 227; 99999; 1005; 0; 256; 1105; 1; 99999; 1106; 227; 99999; 1106; 0; 265; 1105; 1; 99999; 1006; 0; 99999; 1006; 227; 274; 1105; 1; 99999; 1105; 1; 280; 1105; 1; 99999; 1; 225; 225; 225; 1101; 294; 0; 0; 105; 1; 0; 1105; 1; 99999; 1106; 0; 300; 1105; 1; 99999; 1; 225; 225; 225; 1101; 314; 0; 0; 106; 0; 0; 1105; 1; 99999; 7; 677; 226; 224; 102; 2; 223; 223; 1005; 224; 329; 101; 1; 223; 223; 108; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 344; 101; 1; 223; 223; 1108; 226; 677; 224; 102; 2; 223; 223; 1006; 224; 359; 1001; 223; 1; 223; 107; 677; 677; 224; 1002; 223; 2; 223; 1005; 224; 374; 1001; 223; 1; 223; 1107; 226; 677; 224; 102; 2; 223; 223; 1005; 224; 389; 1001; 223; 1; 223; 107; 677; 226; 224; 1002; 223; 2; 223; 1005; 224; 404; 101; 1; 223; 223; 8; 226; 677; 224; 102; 2; 223; 223; 1005; 224; 419; 101; 1; 223; 223; 7; 226; 677; 224; 1002; 223; 2; 223; 1005; 224; 434; 101; 1; 223; 223; 1007; 677; 677; 224; 102; 2; 223; 223; 1006; 224; 449; 1001; 223; 1; 223; 107; 226; 226; 224; 1002; 223; 2; 223; 1006; 224; 464; 1001; 223; 1; 223; 1007; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 479; 1001; 223; 1; 223; 1008; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 494; 101; 1; 223; 223; 7; 677; 677; 224; 102; 2; 223; 223; 1005; 224; 509; 1001; 223; 1; 223; 108; 677; 226; 224; 102; 2; 223; 223; 1005; 224; 524; 101; 1; 223; 223; 1108; 677; 226; 224; 1002; 223; 2; 223; 1006; 224; 539; 101; 1; 223; 223; 1108; 677; 677; 224; 102; 2; 223; 223; 1005; 224; 554; 101; 1; 223; 223; 8; 677; 226; 224; 102; 2; 223; 223; 1005; 224; 569; 101; 1; 223; 223; 8; 677; 677; 224; 102; 2; 223; 223; 1005; 224; 584; 101; 1; 223; 223; 1107; 226; 226; 224; 102; 2; 223; 223; 1006; 224; 599; 101; 1; 223; 223; 108; 677; 677; 224; 102; 2; 223; 223; 1006; 224; 614; 101; 1; 223; 223; 1008; 677; 226; 224; 1002; 223; 2; 223; 1005; 224; 629; 1001; 223; 1; 223; 1107; 677; 226; 224; 102; 2; 223; 223; 1005; 224; 644; 101; 1; 223; 223; 1008; 677; 677; 224; 1002; 223; 2; 223; 1005; 224; 659; 101; 1; 223; 223; 1007; 677; 226; 224; 1002; 223; 2; 223; 1005; 224; 674; 1001; 223; 1; 223; 4; 223; 99; 226  |]
    let result = doCalculation input
    0
