
open System.IO

let rows = File.ReadAllLines "input.txt"
let columnsFromRows (rows : string array) = 
    let n = rows.Length
    [|
        for i in [0..n-1] ->
            [for row in rows -> string row[i]]
            |> String.concat ""
    |]
    // [|0..n-1|] |> Array.map (fun i -> 
    //                 rows
    //                 |> Array.map (fun row -> string row[i] )
    //                 |> String.concat "" )
let diagonals (rows : string array) start = 
    let n = rows.Length
    [|       
        for i in [start..n-1] -> 
            [| for j in [i..n-i-1] -> string (rows[i][j]) |] 
            |> String.concat "" 
    |]


let firstDiagonals = diagonals rows 0
let secondDiagonals = diagonals (columnsFromRows rows) 1
let rowsFlipped = rows |> Array.map (fun row -> string (Array.rev (row.ToCharArray(0,row.Length-1))))
let firstAntiDiagonals = diagonals rowsFlipped 0
let secondAntiDiagonals = diagonals (columnsFromRows rowsFlipped) 1
