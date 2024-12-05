let transpose (rows : string array) = 
    let n = rows.Length
    [|
        for i in [0..n-1] ->
            [for row in rows -> string row[i]]
            |> String.concat ""
    |]

let diagonals (rows : string array) =
    let upperDiagonals (rows : string array) start = 
        let n = rows.Length
        [|       
            for i in [start..n-1] -> 
                [| for j in [0..n-i-1] -> string (rows[j][i + j]) |] 
                |> String.concat "" 
        |]

    [| upperDiagonals rows 0; upperDiagonals (transpose rows) 1|]
    |> Array.concat

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
let everything = 
    [|
        rows;
        transpose rows; // columns
        diagonals rows
        diagonals (Array.map flip rows)
    |] |> Array.concat

printfn "Result: %A" (everything |> Array.sumBy searchAndCount)

let diags = diagonals rows
let antiDiags = diagonals (Array.map flip rows)

let check i (diag : string)= 
    if i < 0 then - 1 else 
        match diag[i-1..i+1] with
        | "MAS" | "SAM" -> i 
        | _ -> -1
let checkAntiDiag (i, j) = 
    let antiDiag = if j >= i then antiDiags[j]
                    else antiDiags[140 + j]
    check i antiDiag

let searchAndCountMas diagIndex (diag: string) = 
        [| 
            for i in [1..diag.Length - 2] ->
                match diag[i] with
                | 'A' -> checkAntiDiag (i, diagIndex)
                | _ -> 0
        |]
        |> Array.sum
                            
let result2 = diags |> Array.mapi searchAndCountMas
                    |> Array.sum

printfn "%A" result2