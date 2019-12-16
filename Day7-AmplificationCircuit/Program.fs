open System
open System.IO
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

type AmplifyMode =
    | Normal
    | Feedback

type StopReason =
    | Output
    | Halt

type Machine = { memory: int list
                 ip: int
                 input: int list
                 output: MachineOutput Option
                 mode: AmplifyMode }
and MachineOutput = { value: int
                      reason: StopReason }

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
    let digits = num |> getIndividualDigits []
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
let updateMemoryLocation memory index value =
    List.mapi (fun i x -> if i = index then value else x ) memory

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
let rec calculateNext (machineState: Machine) =
    let memory = machineState.memory
    let input = machineState.input
    let ip = machineState.ip
    let output = machineState.output
    let mode = machineState.mode
    let operation = memory.Item(ip)
    match getOperation operation with
    | (OperationType.Add, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = updateMemoryLocation memory (o3) (o1 + o2)
                        input = input
                        output = output
                        ip = (ip + 4)
                        mode = mode }
    | (OperationType.Multiply, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = updateMemoryLocation memory (o3) (o1 * o2)
                        input = input
                        output = output
                        ip = (ip + 4) 
                        mode = mode }
    | (OperationType.Store, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 1
        calculateNext { memory = updateMemoryLocation memory (o1) (input.Head)
                        input = input.Tail
                        output = output
                        ip = (ip + 2) 
                        mode = mode }
    | (OperationType.Output, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 2
        let outputState = { memory = memory
                            input = input
                            output = Some { value = o1
                                            reason = StopReason.Output }
                            ip = (ip + 2) 
                            mode = mode }
        match mode with
        | AmplifyMode.Normal -> calculateNext outputState
        | AmplifyMode.Feedback -> outputState
    | (OperationType.JumpIfTrue, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = memory
                        input = input
                        output = output
                        ip = if o1 <> 0 then o2 else (ip + 3) 
                        mode = mode }
    | (OperationType.JumpIfFalse, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = memory
                        input = input
                        output = output
                        ip = if o1 = 0 then o2 else (ip + 3) 
                        mode = mode }
    | (OperationType.LessThan, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = updateMemoryLocation memory (o3) (if o1 < o2 then 1 else 0)
                        input = input
                        output = output
                        ip = (ip + 4) 
                        mode = mode }
    | (OperationType.Equals, modes) ->
        let (o1, o2, o3) = getModes memory ip modes 3
        calculateNext { memory = updateMemoryLocation memory (o3) (if o1 = o2 then 1 else 0)
                        input = input
                        output = output
                        ip = (ip + 4) 
                        mode = mode }
    | (OperationType.Stop, _) -> { machineState with output = Some { value = machineState.output.Value.value
                                                                     reason = StopReason.Halt } }
    | _ -> failwithf "Unsupported operation: %A" operation

/// Start calculating until halt given a certain memory map. Assume Instruction Pointer starts at 0
let doCalculation (machineState: Machine) =
    calculateNext machineState
    
let rec amplifyNext (machines: Machine list) (phaseSettings: List<int>) (results: Machine list) =
    match machines with
    | x::y::xs ->
        let newState = doCalculation { x with input =  ( (if phaseSettings.IsEmpty then [] else [ phaseSettings.Head ])@ x.input) }
        let output = match newState.output with
                     | Some out -> [ out.value ]
                     | None -> [ ]
        let outResult = amplifyNext ({ y with input = output } :: xs) (if phaseSettings.IsEmpty then [] else phaseSettings.Tail) results
        [newState] @ outResult
    | x::[] -> 
        let newState = doCalculation { x with input =  ((if phaseSettings.IsEmpty then [] else [ phaseSettings.Head ]) @ x.input) }
        [newState]
    | [] -> []

let rec amplifyNextFeedback (machines: Machine list) (phaseSettings: List<int>) (results: Machine list) =
    match amplifyNext machines phaseSettings results with
    | xs ->
        let x::y = xs
        if (List.last xs).output.Value.reason = StopReason.Output then amplifyNextFeedback ({ x with input = [(List.last xs).output.Value.value] } :: y) [] [] else xs
    

let amplify (machineState: Machine) (phaseSettings: List<int>) (mode: AmplifyMode)  =
    let machineList = List.init (List.length phaseSettings) (fun _ -> { machineState with ip = 0 })
    let initial = machineList.Head
    let machines = { initial with input = [ 0 ] } :: machineList.Tail
    if mode = AmplifyMode.Normal then amplifyNext machines phaseSettings [] else amplifyNextFeedback machines phaseSettings []
    
let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let getSearchspace (amplifiers: int) =
    [0 .. amplifiers-1]
    |> permutations

let getSearchspaceFeedback (amplifiers: int) =
    [5 .. amplifiers + 4 ]
    |> permutations
    
let calculateLargestResult (machineState: Machine) (searchspace: seq<int list>) =
    searchspace
    |> Seq.map (fun x -> List.last (amplify machineState (Seq.toList x) machineState.mode))
    |> Seq.map (fun x -> x.output)
    |> Seq.filter (fun x -> x.IsSome)
    |> Seq.map (fun x -> x.Value)
    |> Seq.max

let readInput path =
    File.ReadAllText(path).Split(',')
    |> Array.map (fun x -> int x)
    |> Array.toList
  
[<EntryPoint>]
let main argv =
    let (input, mode) = match argv with
                        | [| path; mode |] -> (readInput path, match mode with
                                                               | "normal" -> AmplifyMode.Normal
                                                               | "feedback" -> AmplifyMode.Feedback
                                                               | _ -> AmplifyMode.Normal)
                        | _ -> failwithf "Usage: dotnet run <path-to-input-file> <mode? := normal|feedback>"
    let max = match mode with
              | AmplifyMode.Normal -> getSearchspace 5
              | AmplifyMode.Feedback -> getSearchspaceFeedback 5
    let initialState = { memory = input
                         ip = 0
                         input = []
                         output = None
                         mode = mode }
    printfn "Amplified output: %d" (calculateLargestResult initialState max).value
    0
