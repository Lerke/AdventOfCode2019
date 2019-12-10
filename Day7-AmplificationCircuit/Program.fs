open System
open FSharp.Collections.ParallelSeq

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
let rec calculateNext (memory:List<int>) (input: List<int>) (output: List<int>) ip =
    let operation = memory.Item(ip)
    match getOperation operation with
    | (OperationType.Add, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext (updateMemoryLocation memory (o3) ((o1 + o2))) input output (ip + 4)
    | (OperationType.Multiply, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext (updateMemoryLocation
                            memory
                            (o3) ((o1 * o2))) input output (ip + 4)
    | (OperationType.Store, modes) ->
        let r_input = input.Head
        let newInput = input.Tail
        let (o1, o2, o3) = getModes memory ip modes 1
        calculateNext (updateMemoryLocation
                            memory
                            (memory.Item(ip+1)) (r_input)) newInput output (ip + 2)
    | (OperationType.Output, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 2
        let newOutput = output @ [ o1 ]
        calculateNext memory input newOutput (ip + 2)
    | (OperationType.JumpIfTrue, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let newIp = if o1 <> 0 then o2 else (ip + 3)
        calculateNext memory input output newIp
    | (OperationType.JumpIfFalse, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let newIp = if o1 = 0 then o2 else (ip + 3)
        calculateNext memory input output newIp
    | (OperationType.LessThan, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let result = if o1 < o2 then 1 else 0
        calculateNext (updateMemoryLocation
                            memory
                            (o3) (result)) input output (ip + 4)
    | (OperationType.Equals, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        let result = if o1 = o2 then 1 else 0
        calculateNext (updateMemoryLocation
                            memory
                            (o3) (result)) input output (ip + 4)
    | (OperationType.Stop, _) -> ( memory, output )

/// Start calculating until halt given a certain memory map. Assume Instruction Pointer starts at 0
let doCalculation (memory:List<int>) (input: List<int>) =
    calculateNext memory input [] 0
    
let rec amplifyNext (program: List<int>) (input: List<int>) (phaseSettings: List<int>) =
    match phaseSettings with
    | x::xs ->
        let (_, output) = doCalculation program ([ x ] @ input)
        amplifyNext program output xs
    | [] -> input
    
let amplify (program: List<int>) (phaseSettings: List<int>)  =
    amplifyNext program [0] phaseSettings

let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let getSearchspace (amplifiers: int) =
    [0 .. amplifiers-1]
    |> permutations
    
let calculateLargestResult (searchspace: seq<int list>) (input: List<int>) =
    searchspace
    |> PSeq.map (fun x -> List.last (amplify input (Seq.toList x)))
    |> PSeq.max
  
[<EntryPoint>]
let main argv =
    let input = Array.toList [| 3;8;1001;8;10;8;105;1;0;0;21;38;59;76;89;106;187;268;349;430;99999;3;9;1002;9;3;9;101;2;9;9;1002;9;4;9;4;9;99;3;9;1001;9;5;9;1002;9;5;9;1001;9;2;9;1002;9;3;9;4;9;99;3;9;1001;9;4;9;102;4;9;9;1001;9;3;9;4;9;99;3;9;101;4;9;9;1002;9;5;9;4;9;99;3;9;1002;9;3;9;101;5;9;9;1002;9;3;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;99;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;99 |]
    let max = getSearchspace 5
    printfn "Amplified output: %d" (calculateLargestResult max input)
    0
