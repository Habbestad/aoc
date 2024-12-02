
open System.Text.RegularExpressions
let parseLine str = 
    Regex.Split(str, @"\s+") |> Array.map int

let isSafe report = 
    let pairs = Array.pairwise report

    (pairs |> Array.forall (fun (a, b) -> a < b) 
    || 
    pairs |> Array.forall (fun (a, b) -> a > b))
    &&
    pairs |> Array.forall (fun (a, b) -> 0 < abs(a-b) && abs(a-b) < 4)

let result1 = "input.txt" 
              |> System.IO.File.ReadAllLines
              |> Array.map (parseLine >> isSafe)
              |> Array.filter id
              |> Array.length

printfn "Problem 1: %A" result1
