module Tests

open FsUnit
open Program
open Xunit

[<Fact>]
let ``simple example`` () =
    let testProgram = [1;0;0;0;99]
    let testOutput = [2;0;0;0;99]
    let result = doCalculation testProgram
    result |> should equal testOutput


[<Fact>]
let ``simple example two`` () =
    let testProgram = [2;3;0;3;99]
    let testOutput = [2;3;0;6;99]
    let result = doCalculation testProgram
    result |> should equal testOutput 
    
[<Fact>]
let ``simple example three`` () =
    let testProgram = [2;4;4;5;99;0]
    let testOutput = [2;4;4;5;99;9801]
    let result = doCalculation testProgram
    result |> should equal testOutput

[<Fact>]
let ``simple example four`` () =
    let testProgram = [1;1;1;4;99;5;6;0;99]
    let testOutput = [30;1;1;4;2;5;6;0;99]
    let result = doCalculation testProgram
    result |> should equal testOutput

[<Fact>]
let ``example`` () =
    let testProgram = [1;9;10;3;2;3;11;0;99;30;40;50]
    let testOutput = [3500;9;10;70;2;3;11;0;99;30;40;50 ]
    let result = doCalculation testProgram
    result |> should equal testOutput

