open System
open System.IO

/// Calculate the required fuel for given spaceship module with a certain mass.
let rec calculateModuleFuel mass =
    let fuelToUse = (Convert.ToInt32(floor (float (mass) / 3.0))) - 2
    if fuelToUse > 0
        then fuelToUse + calculateModuleFuel fuelToUse
        else 0 // Wishing really hard!
    

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
        printfn "Please input a number."
        1