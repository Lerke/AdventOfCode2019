open FSharp.Text.RegexProvider

(*
--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet these criteria?

Your puzzle input is 402328-864247.
*)

// I snipe both numbers given as input with a regular expression.
type inputRegex = Regex< @"^(?<number>\d+)\-(?<number2>\d+)$" >

let passwordIsNDigits (n: int) (password: int list) =
    password.Length = n

// Given a list of numbers, check if it has an occurence of double digits.
let rec passwordHasAdjacentDigits (password: int list) =
    match password with
    | x::y::xs when x = y -> true
    | x::y::xs when x <> y -> passwordHasAdjacentDigits (y::xs)
    | _ -> false

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
        |> fun x -> printfn "Number of passwords between %d - %d that fit the criteria: %d" number number2 x.Length
    | _ -> printf "Usage: dotnet run <lower bound:int> <upper bound:int>"
    0
