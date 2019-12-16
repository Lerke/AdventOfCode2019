module Tests

open System.Linq
open FsUnit
open Program
open Program
open Xunit

[<Fact>]
let ``Test Image Parsing I`` () =
    let imageData = seq { 1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2 }
    let width = 3
    let height = 2
    let image = buildImage imageData width height
    
    image.height |> should equal height
    image.width |> should equal width
    image.layers.Length |> should equal 2
    
    image.layers.Item(0).layerNumber |> should equal 1
    image.layers.Item(0).rows.Length |> should equal 2
    image.layers.Item(0).rows.Item(0).rowNumber |> should equal 1
    Enumerable.SequenceEqual ((image.layers.Item(0).rows.Item(0).pixels), (seq {1;2;3})) |> should equal true
    image.layers.Item(0).rows.Item(1).rowNumber |> should equal 2
    Enumerable.SequenceEqual ((image.layers.Item(0).rows.Item(1).pixels), (seq {4;5;6})) |> should equal true
    
    image.layers.Item(1).layerNumber |> should equal 2
    image.layers.Item(1).rows.Length |> should equal 2
    image.layers.Item(1).rows.Item(0).rowNumber |> should equal 1
    Enumerable.SequenceEqual ((image.layers.Item(1).rows.Item(0).pixels), (seq {7;8;9})) |> should equal true
    image.layers.Item(1).rows.Item(1).rowNumber |> should equal 2
    Enumerable.SequenceEqual ((image.layers.Item(1).rows.Item(1).pixels), (seq {0;1;2})) |> should equal true

[<Theory>]
[<InlineData(0, 1)>]
[<InlineData(1, 1)>]
[<InlineData(2, 1)>]
[<InlineData(3, 2)>]
[<InlineData(4, 2)>]
[<InlineData(5, 2)>]
[<InlineData(6, 2)>]
[<InlineData(7, 1)>]
[<InlineData(8, 1)>]
[<InlineData(9, 1)>]
let ``Test Layer With Fewest N`` (n, expectedLayer) =
    let imageData = seq { 1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2 }
    let width = 3
    let height = 2
    let image = buildImage imageData width height
    let (layerNumber, _) = layerWithFewestN image n
    layerNumber |> should equal expectedLayer
    
[<Fact>]
let ``Test Finding Ones and Twos``() =
    let imageData = seq { 1; 2; 3; 4; 5; 6; 7; 8; 9; 0; 1; 2 }
    let width = 3
    let height = 2
    let image = buildImage imageData width height
    let (layerNumber, _) = layerWithFewestN image 0
    let result = productOfOnesAndTwos image layerNumber
    result |> should equal 1