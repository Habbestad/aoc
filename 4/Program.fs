let transpose (rows : string array) = 
    let n = rows.Length
    [|
        for i in [0..n-1] ->
            [for row in rows -> string row[i]]
            |> String.concat ""
    |]

let upperDiagonals (rows : string array) start = 
    let n = rows.Length
    [|       
        for i in [start..n-1] -> 
            [| for j in [0..n-i-1] -> string (rows[j][i + j]) |] 
            |> String.concat "" 
    |]

let searchAndCount (str : string) = 
    let check i = 
        let k = match str[i..(min (i+3) str.Length)] = "XMAS" with
                | true -> 1
                | false -> 0
        let l = match str[(max (i-3) 0) .. i] = "SAMX" with
                | true -> 1
                | false -> 0
        k + l

    [|0 .. str.Length-1|] 
    |> Array.fold (fun sum i -> 
            match str[i] with
            | 'X' -> sum + check i 
            | _ ->  sum + 0 ) 0

let flip (a : string) = [| for i in a.Length - 1 .. -1 .. 0 -> string a[i]|] 
                        |> String.concat ""

let rows = System.IO.File.ReadAllLines "input.txt"
let rowsFlipped = rows |> Array.map flip
let everything = 
    [|
        rows;
        transpose rows; // columns

        upperDiagonals rows 0; // upper diagonals
        upperDiagonals (transpose rows) 1; // lower diagonals

        upperDiagonals rowsFlipped 0; // upper antidiagonals
        upperDiagonals (transpose rowsFlipped) 1; // lower antidiagonals
    |] |> Array.concat

printfn "Result: %A" (everything |> Array.sumBy searchAndCount)