let transpose (rows : string array) = 
    let n = rows.Length
    [|
        for i in [0..n-1] ->
            [for row in rows -> string row[i]]
            |> String.concat ""
    |]
let upperDiagonals (rows : string array) offset = 
    let n = rows.Length
    [|       
        for i in [offset .. n-1] -> 
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

let rows = System.IO.File.ReadAllLines "testdata.txt"
let columns = transpose rows
let upperDiags = upperDiagonals rows 0
let lowerDiags = upperDiagonals columns 1
let rowsFlipped = Array.map flip rows
let upperAntiDiags = upperDiagonals rowsFlipped 0
let lowerAntiDiags = upperDiagonals (transpose rowsFlipped) 1


let everything = 
    [|
        rows;
        transpose rows; // columns
        upperDiags
        lowerDiags
        upperAntiDiags
        lowerAntiDiags
    |] |> Array.concat

printfn "Result: %A" (everything |> Array.sumBy searchAndCount)

let n = rows.Length;
let check (i, j)= 
    let c1,d1 = 
        if j >= i 
        then i, upperDiags[j - i]
        else j, lowerDiags[i - j-1]

    let c2, d2 = i, upperAntiDiags[j-1] //hmm

    let containsMas ci (str : string) =  
        match str[ci - 1 .. ci + 1] with
        | "MAS" | "SAM" -> 1 
        | _ -> 0

    containsMas c1 d1 * containsMas c2 d2

let searchAndCountMas rowIndex (row: string) = 
        [| 
            for colIndex in [1 .. row.Length - 2] ->
                match row[colIndex] with
                | 'A' -> check (rowIndex, colIndex)
                | _ -> 0
        |]
        |> Array.sum
                            
let result2 = rows |> Array.mapi searchAndCountMas
                   |> Array.sum

printfn "%A" result2