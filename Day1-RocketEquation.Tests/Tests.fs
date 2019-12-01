module Tests

open FsUnit
open Program
open Xunit

[<Theory>]
[<InlineData(14, 2)>]
[<InlineData(1969, 966)>]
[<InlineData(100756, 50346)>]
let ``Test Calculate Module Fuel`` (test, expected) =
    let actual = calculateModuleFuel test
    actual |> should equal expected

[<Fact>]
let ``Test Calculate Spaceship Fuel`` () =
    let testValues = [|14; 1969; 100756;|]
    let expected = 2 + 966 + 50346
    let actual = calculateSpaceshipFuel testValues
    actual |> should equal expected
    