module Tests

open FsUnit
open Program
open Program
open Xunit

[<Fact>]
let ``Test Amplify``() =
    let program = [ 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 ]
    let sequence = [ 4; 3; 2; 1; 0 ]
    let expected = 43210
    let result = amplify { memory = program
                           input = []
                           output = None
                           ip = 0
                           mode = AmplifyMode.Normal } sequence AmplifyMode.Normal
    (List.last result).output.Value.value |> should equal expected

[<Fact>]
let ``Test Self Amplify``() =
    let program = [ 3; 15; 3; 16; 1002; 16; 10; 16; 1; 16; 15; 15; 4; 15; 99; 0; 0 ]
    let expected = 43210
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Normal } (getSearchspace 5) 
    result.value |> should equal expected

[<Fact>]
let ``Test Amplify II``() =
    let program = [ 3; 23; 3; 24; 1002; 24; 10; 24; 1002; 23; -1; 23; 101; 5; 23; 23; 1; 24; 23; 23; 4; 23; 99; 0; 0 ]
    let sequence = [ 0; 1; 2; 3; 4 ]
    let expected = 54321
    let result = amplify { memory = program
                           input = []
                           output = None
                           ip = 0
                           mode = AmplifyMode.Normal } sequence AmplifyMode.Normal
    (List.last result).output.Value.value |> should equal expected

[<Fact>]
let ``Test Self Amplify II``() =
    let program = [ 3; 23; 3; 24; 1002; 24; 10; 24; 1002; 23; -1; 23; 101; 5; 23; 23; 1; 24; 23; 23; 4; 23; 99; 0; 0 ]
    let expected = 54321
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Normal } (getSearchspace 5) 
    result.value |> should equal expected

[<Fact>]
let ``Test Amplify III``() =
    let program = [ 3; 31; 3; 32; 1002; 32; 10; 32; 1001; 31; -2; 31; 1007; 31; 0; 33; 1002; 33; 7; 33; 1; 33; 31; 31; 1; 32; 31; 31; 4; 31; 99; 0; 0; 0 ]
    let sequence = [ 1; 0; 4; 3; 2 ]
    let expected = 65210
    let result = amplify { memory = program
                                    input = []
                                    output = None
                                    ip = 0
                                    mode = AmplifyMode.Normal } sequence AmplifyMode.Normal
    (List.last result).output.Value.value |> should equal expected

[<Fact>]
let ``Test Self Amplify III``() =
    let program = [ 3; 31; 3; 32; 1002; 32; 10; 32; 1001; 31; -2; 31; 1007; 31; 0; 33; 1002; 33; 7; 33; 1; 33; 31; 31; 1; 32; 31; 31; 4; 31; 99; 0; 0; 0 ]
    let expected = 65210
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Normal } (getSearchspace 5) 
    result.value |> should equal expected

[<Fact>]
let ``Test Task I``() =
    let program = Array.toList [| 3;8;1001;8;10;8;105;1;0;0;21;38;59;76;89;106;187;268;349;430;99999;3;9;1002;9;3;9;101;2;9;9;1002;9;4;9;4;9;99;3;9;1001;9;5;9;1002;9;5;9;1001;9;2;9;1002;9;3;9;4;9;99;3;9;1001;9;4;9;102;4;9;9;1001;9;3;9;4;9;99;3;9;101;4;9;9;1002;9;5;9;4;9;99;3;9;1002;9;3;9;101;5;9;9;1002;9;3;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;99;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;99 |]
    let expected = 199988
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Normal } (getSearchspace 5)
    result.value |> should equal expected
    
[<Fact>]
let ``Test Task II``() =
    let program = Array.toList [| 3;8;1001;8;10;8;105;1;0;0;21;38;59;76;89;106;187;268;349;430;99999;3;9;1002;9;3;9;101;2;9;9;1002;9;4;9;4;9;99;3;9;1001;9;5;9;1002;9;5;9;1001;9;2;9;1002;9;3;9;4;9;99;3;9;1001;9;4;9;102;4;9;9;1001;9;3;9;4;9;99;3;9;101;4;9;9;1002;9;5;9;4;9;99;3;9;1002;9;3;9;101;5;9;9;1002;9;3;9;4;9;99;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;1;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;102;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;99;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;3;9;101;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;1002;9;2;9;4;9;3;9;101;1;9;9;4;9;3;9;101;1;9;9;4;9;99;3;9;101;2;9;9;4;9;3;9;102;2;9;9;4;9;3;9;1002;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;1;9;4;9;3;9;1001;9;2;9;4;9;3;9;1001;9;2;9;4;9;3;9;102;2;9;9;4;9;99 |]
    let expected = 17519904
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Feedback } (getSearchspaceFeedback 5)
    result.value |> should equal expected

[<Fact>]
let ``Test Amplified Test I``() =
    let program = Array.toList [| 3;26;1001;26;-4;26;3;27;1002;27;2;27;1;27;26;27;4;27;1001;28;-1;28;1005;28;6;99;0;0;5  |]
    let expected = 139629729
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Feedback } (getSearchspaceFeedback 5)
    result.value |> should equal expected
    
[<Fact>]
let ``Test Amplified Test II``() =
    let program = Array.toList [| 3;52;1001;52;-5;52;3;53;1;52;56;54;1007;54;5;55;1005;55;26;1001;54;-5;54;1105;1;12;1;53;54;53;1008;54;0;55;1001;55;1;55;2;53;55;53;4;53;1001;56;-1;56;1005;56;6;99;0;0;0;0;10 |]
    let expected = 18216
    let result = calculateLargestResult { memory = program
                                          input = []
                                          output = None
                                          ip = 0
                                          mode = AmplifyMode.Feedback } (getSearchspaceFeedback 5)
    result.value |> should equal expected
