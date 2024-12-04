
open System.IO

let rows = File.ReadAllLines "input.txt"
let n = rows.Length


let columnsFromRows (rows : string array) = 
    [|0..n|] |> Array.map (fun i -> 
                    rows
                    |> Array.map (fun row -> string row[i] )
                    |> String.concat "" )
let diagonals (rows : string array) start = 
    [|start..n-4|] |> Array.map (fun i -> 
                    rows[i..i+4] 
                    |> Array.mapi (fun j r -> string r[i+j])
                    |> String.concat "")

let firstDiagonals = diagonals rows 0
let secondDiagonals = diagonals (columnsFromRows rows) 1
let rowsFlipped = rows |> Array.map (fun row -> string (Array.rev (row.ToCharArray(0,n))))
let firstAntiDiagonals = diagonals rowsFlipped 0
let secondAntiDiagonals = diagonals (columnsFromRows rowsFlipped) 1

printfn "%A" secondDiagonals

