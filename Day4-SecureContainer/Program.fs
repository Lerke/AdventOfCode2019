open FSharp.Text.RegexProvider

// I snipe both numbers given as input with a regular expression.
type inputRegex = Regex< @"^(?<number>\d+)\-(?<number2>\d+)$" >

let passwordIsNDigits (n: int) (password: int list) =
    password.Length = n

// Given a number, fast forward through this list until we reach the end or a different number
// Return the new list from that point, including the last occurence of number.
let rec fastForward number list =
    match list with
    | x::y::xs when x = number && y = number -> fastForward number (y::xs)
    | x::y::xs when x = number && y <> number -> (number::y::xs)
    | x::[] -> []
    | [] -> []

// Given a list of numbers, check if it has an occurence of double digits.
let rec passwordHasAdjacentDigits (password: int list) =
    match password with
    | x::y::z::xs when x = y && y <> z -> true                             // This is a isolated group of two. We don't have to search further.
    | x::y::z::[] when x <> y && y = z -> true                             // Capture groups at the very end
    | x::y::z::xs when x = y && y = z -> passwordHasAdjacentDigits (fastForward x password) // This group is too large, skip ahead.
    | x::y::xs when x <> y -> passwordHasAdjacentDigits (y::xs)            // No match, seek further
    | _ -> false                                                           // No result found.

// Given a list of numbers, check if each consecutive number is equal or greater than the previous.
let rec passwordIsAscending (highest:int) (password: int list) =
    match password with
    | x::xs when x >= highest -> passwordIsAscending x xs
    | x::xs when x < highest -> false
    | x::[] -> true
    | [] -> true

// Given a number, return an array containing each individual number.
let rec getIndividualDigits input number =
    match number with
    | x when x > 0 -> getIndividualDigits (List.append input [ x % 10 ]) (x / 10)
    | x ->
        input
        |> List.rev

// Given a password, check if it meets our criteria.
let meetsCriteria password =
    password
    |> (getIndividualDigits [])
    |> fun password -> passwordIsAscending 0 password
                       && passwordIsNDigits 6 password
                       && passwordHasAdjacentDigits password

// Parses input in the form of 'number-number2' to a tuple (number, number2).
let parseInput input =
    match inputRegex().TryTypedMatch(input) with
    | Some m -> (int m.number.Value, int m.number2.Value)
    | None -> failwith "Could not parse input."
    
[<EntryPoint>]
let main argv =
    let mutable (number, number2) = (0,0)
    match argv with
    | [| input |] ->
        parseInput input
        |> fun (n, n2) ->
            number <- n; number2 <- n2
            [ number .. number2 ]
        |> List.filter (fun x -> meetsCriteria x)
        |> fun x -> printfn "Number of passwords between %d - %d that fit the criteria: %d" number number2 x.Length; x
        |> fun x -> List.iter (fun password -> printfn "%d" password) x
    | _ -> printf "Usage: dotnet run <lower bound:int> <upper bound:int>"
    0
