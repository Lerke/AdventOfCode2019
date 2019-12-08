module Tests

open FsUnit
open Program
open Xunit

[<Fact>]
let ``Test Get Individual Numbers``() =
    [ (123456, [ 1; 2; 3; 4; 5; 6 ])
      (111111, [ 1; 1; 1; 1; 1; 1 ])
      (223450, [ 2; 2; 3; 4; 5; 0 ])
      (123789, [ 1; 2; 3; 7; 8; 9 ])
      (122345, [ 1; 2; 2; 3; 4; 5 ]) ]
    |> List.iter (fun (number, expected) -> getIndividualDigits [] number |> should equal expected)

[<Fact>]
let ``Test Parse Numbers``() =
    [ ("1000-2000", (1000, 2000))
      ("1-2", (1, 2))
      ("402328-864247", (402328, 864247)) ]
    |> List.iter (fun (number, expected) -> parseInput number |> should equal expected)

[<Fact>]
let ``Test Ascending Passwords``() =
    [ [ 1; 2; 3; 4; 5; 6 ]
      [ 1; 1; 2; 2; 3; 3 ]
      [ 4; 4; 6; 6; 6; 7 ] ]
    |> List.iter (fun numbers -> passwordIsAscending 0 numbers |> should equal true)

[<Fact>]
let ``Test Failing Ascending Passwords``() =
    [ [ 1; 2; 1; 4; 5; 6 ]
      [ 1; 1; 2; 2; 8; 3 ]
      [ 4; 4; 6; 6; 6; 4 ] ]
    |> List.iter (fun numbers -> passwordIsAscending 0 numbers |> should equal false)

[<Fact>]
let ``Test Adjacent Digits Passwords``() =
    [ [ 1; 1; 3; 4; 5; 6 ]
      [ 1; 1; 2; 2; 3; 3 ]
      [ 4; 5; 6; 7; 8; 8 ] ]
    |> List.iter (fun numbers -> passwordHasAdjacentDigits numbers |> should equal true)


[<Fact>]
let ``Test Failing Adjacent Digits Passwords``() =
    [ [ 1; 2; 3; 4; 5; 6 ]
      [ 1; 3; 4; 5; 6; 9 ]
      [ 4; 5; 6; 7; 8; 9 ] ]
    |> List.iter (fun numbers -> passwordHasAdjacentDigits numbers |> should equal false)

[<Fact>]
let ``Test Fast Forward``() =
    [ ([ 1; 1; 1; 1; 4; 4 ], 1) ]
    |> List.iter (fun (numbers, expected) -> (fastForward expected numbers) |> should equal [1; 4; 4])

[<Fact>]
let ``Test Failing Criteria Matches``() =
    [ 223450; 123789; 111111; 123444 ]
    |> List.iter (fun numbers -> meetsCriteria numbers |> should equal false)


[<Fact>]
let ``Test Criteria Matches Part 1``() =
    [ 122345; 135677; 111122 ]
    |> List.iter (fun numbers -> meetsCriteria numbers |> should equal true)
