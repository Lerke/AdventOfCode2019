open System
open System.IO

/// Calculate the required fuel for given spaceship module with a certain mass.
let rec calculateModuleFuel mass =
    mass
    |> float
    |> fun fuel -> fuel / 3.        // Divide mass by three
    |> floor                        // Floor division
    |> Convert.ToInt32              // Go back to int.
    |> fun fuel -> fuel - 2         // Subtract 2
    |> fun fuel -> match fuel with  // If amount of fuel is 0 or less, return 0. Else, calculate extra mass.
                    | f when f <= 0 -> 0 
                    | f -> f + calculateModuleFuel f

/// Read an input file, convert every line to a whole number.
let loadInputFile filepath =
    File.ReadAllLines(filepath)
    |> Seq.filter (fun x -> Int32.TryParse x |> fst) // Only keep numbers!
    |> Seq.map (fun x -> int x) // Cast strings to numbers

/// Calculate the required fuel of all modules and sum.
let calculateSpaceshipFuel modules =
    modules
    |> Seq.map (fun x -> calculateModuleFuel x)
    |> Seq.sum

/// Application takes a single parameter as input.
/// Param 1: Path to input file from adventofcode.com
[<EntryPoint>]
let main argv =
    match argv with
    | [| file |] ->
        let modules = loadInputFile file
        let spaceshipFuel = calculateSpaceshipFuel modules
        printfn "Total space ship fuel: %d" spaceshipFuel
        0
    | _ ->
        printfn "Please input path to input file."
        1
