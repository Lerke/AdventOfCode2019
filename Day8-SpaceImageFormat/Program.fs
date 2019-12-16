open FSharp.Collections.ParallelSeq
open FSharp.Collections.ParallelSeq
open FSharp.Collections.ParallelSeq
open FSharp.Collections.ParallelSeq
open FSharp.Collections.ParallelSeq
open System
open System.IO

type PixelColor =
    | Black
    | White
    | Transparent

type FinalImage = {
   width: int
   height: int
   pixels: PixelColor list
}
and Image = {
    width: int
    height: int
    layers: Layer list
}
and Layer = {
    layerNumber: int
    rows: ImageRow list
}
and ImageRow = {
    rowNumber: int
    pixels: int seq
}

let rec buildRows (row: int seq) (width: int) (depth: int) =
    match Seq.isEmpty row with
    | true -> []
    | false ->
        let newrow = { rowNumber = depth
                       pixels = (Seq.take width row) }
        [newrow] @ buildRows (Seq.skip width row) width (depth + 1)

let rec buildLayers (data: int seq) (width: int) (height: int) (depth: int) =
    let layer = Seq.take (width * height) data
    let rows = buildRows layer width 1
    let newLayer = { layerNumber = depth
                     rows = rows }
    match Seq.isEmpty (Seq.skip (width * height) data) with
    | true -> [newLayer]
    | false -> [newLayer] @ buildLayers (Seq.skip (width * height) data) width height (depth + 1)


let buildImage (data: int seq) (width: int) (height: int) =
    let layers = buildLayers data width height 1
    { width = width
      height = height
      layers = layers }
    
let countDigits digits n =
    PSeq.fold (fun acc x -> if x = n then (acc + 1) else acc) 0 digits

let layerWithFewestN image n =
    image.layers
    |> PSeq.map (fun x ->
                        let r = (x.layerNumber, x.rows |> PSeq.fold (fun acc x -> Seq.append x.pixels acc) (seq[]) )
                        r)
    |> PSeq.map (fun (layer, digits) -> (layer, countDigits digits n))
    |> PSeq.minBy (fun (_, digits) -> digits)
    
let productOfOnesAndTwos image layerNumber =
    let layer = List.find (fun x -> x.layerNumber = layerNumber) image.layers
    layer.rows
    |> PSeq.fold (fun acc x -> Seq.append acc x.pixels) (seq[])
    |> PSeq.fold (fun (ones,twos) x -> (if x = 1 then (ones+1, twos) else if x = 2 then (ones, twos + 1) else (ones, twos))) (0,0)
    |> (fun (ones,twos) -> ones * twos)
    
let normalizePixelRows (rows: ImageRow list) =
    rows
    |> Seq.fold (fun pixels x -> Seq.append pixels x.pixels) (seq[])
    
let rec pixelColor (colors: int seq) =
    match Seq.toList colors with
    | x::xs -> if x = 0 then PixelColor.Black
                                  else if x = 1 then PixelColor.White
                                  else if x = 2 then pixelColor xs
                                  else PixelColor.Transparent
    | [] -> PixelColor.Transparent
let calculateLayerPixel (layers:Layer list) index =
    layers
    |> Seq.map (fun x -> normalizePixelRows x.rows)
    |> Seq.map (fun x -> Seq.head (Seq.skip (index) x))
    |> pixelColor
    
let calculateFinalImage (image: Image) =
    { width = image.width
      height = image.height
      pixels = [0..(image.height * image.width - 1)]
                |> List.mapi (fun i x -> (i,x))
                |> PSeq.map (fun (i,x) -> (i, calculateLayerPixel image.layers x))
                |> PSeq.toList
                |> List.sortBy (fun (i,_) -> i)
                |> List.map snd }

let printFinalImageToConsole (image: FinalImage) =
    printfn "Final Image"
    for h in [0..image.height - 1] do
        for w in [0..image.width - 1] do
            printf "%s" (match image.pixels.Item((h * image.width) + w) with
                         | PixelColor.White -> "■"
                         | PixelColor.Black -> "□"
                         | PixelColor.Transparent -> " ")
        printf "\n"

let parseInput path =
    File.ReadAllText(path).ToCharArray()
    |> Array.map (fun x -> int (string x))
    |> Array.toSeq

[<EntryPoint>]
let main argv =
    let (input, width, height)  = match argv with
                                  | [| imagePath; width;  height |] -> (parseInput imagePath, int width, int height)
                                  | _ -> failwith "Usage: dotnet run <path-to-input:string> <width:int> <height:int>"
    let image = buildImage input width height
    let (minLayer, _) = layerWithFewestN image 0
    let product = productOfOnesAndTwos image minLayer
    printfn "Number of 1 and 2 digits muliplied of layer with fewest 0 digits: [Layer: %d, Sum: %d]" minLayer product
    let finalImage = calculateFinalImage image
    printFinalImageToConsole finalImage
    0 // return an integer exit code