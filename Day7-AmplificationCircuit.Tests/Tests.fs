module Tests

open FsUnit
open Program
open System
open Xunit

[<Fact>]
let ``Test Amplify``() =
    let program = [ 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 ]
    let sequence = [ 4; 3; 2; 1; 0 ]
    let expected = 43210
    let result = amplify { memory = program
                           input = []
                           output = None
                           ip = 0 } sequence
    result.output.Value |> should equal expected

[<Fact>]
let ``Test Self Amplify``() =
    let program = [ 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 ]
    let expected = 43210
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0 } (getSearchspace 5) 
    result |> should equal expected

[<Fact>]
let ``Test Amplify II``() =
    let program = [ 3; 23; 3; 24; 1002; 24; 10; 24; 1002; 23; -1; 23; 101; 5; 23; 23; 1; 24; 23; 23; 4; 23; 99; 0; 0 ]
    let sequence = [ 0; 1; 2; 3; 4 ]
    let expected = 54321
    let result = amplify { memory = program
                           input = []
                           output = None
                           ip = 0 } sequence
    result.output.Value |> should equal expected

[<Fact>]
let ``Test Self Amplify II``() =
    let program = [ 3; 23; 3; 24; 1002; 24; 10; 24; 1002; 23; -1; 23; 101; 5; 23; 23; 1; 24; 23; 23; 4; 23; 99; 0; 0 ]
    let expected = 54321
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0 } (getSearchspace 5) 
    result |> should equal expected

[<Fact>]
let ``Test Amplify III``() =
    let program = [ 3; 31; 3; 32; 1002; 32; 10; 32; 1001; 31; -2; 31; 1007; 31; 0; 33; 1002; 33; 7; 33; 1; 33; 31; 31; 1; 32; 31; 31; 4; 31; 99; 0; 0; 0 ]
    let sequence = [ 1; 0; 4; 3; 2 ]
    let expected = 65210
    let result = amplify { memory = program
                                    input = []
                                    output = None
                                    ip = 0 } sequence
    result.output.Value |> should equal expected

[<Fact>]
let ``Test Self Amplify III``() =
    let program = [ 3; 31; 3; 32; 1002; 32; 10; 32; 1001; 31; -2; 31; 1007; 31; 0; 33; 1002; 33; 7; 33; 1; 33; 31; 31; 1; 32; 31; 31; 4; 31; 99; 0; 0; 0 ]
    let expected = 65210
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0 } (getSearchspace 5) 
    result |> should equal expected

