open System
open System.IO

type Image = {
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
    Seq.fold (fun acc x -> if x = n then (acc + 1) else acc) 0 digits

let layerWithFewestN image n =
    image.layers
    |> List.map (fun x ->
                        let r = (x.layerNumber, x.rows |> List.fold (fun acc x -> Seq.append x.pixels acc) (seq[]) )
                        r)
    |> List.map (fun (layer, digits) -> (layer, countDigits digits n))
    |> List.minBy (fun (_, digits) -> digits)
    
let productOfOnesAndTwos image layerNumber =
    let layer = List.find (fun x -> x.layerNumber = layerNumber) image.layers
    layer.rows
    |> List.fold (fun acc x -> Seq.append acc x.pixels) (seq[])
    |> Seq.fold (fun (ones,twos) x -> (if x = 1 then (ones+1, twos) else if x = 2 then (ones, twos + 1) else (ones, twos))) (0,0)
    |> (fun (ones,twos) -> ones * twos)
    
    
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
    0 // return an integer exit code