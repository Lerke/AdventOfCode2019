module Array

// Executes a fold operation within a list passing as parameter of the folder function 
// the zero based index of each element.
// I shamelessly ripped this from stack overflow.
let public foldi fold first source  =
    source 
    |> Array.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0)
    |> fst
