open System
open System.Text.RegularExpressions

let parseLine line  = 
    let pair = Regex.Split(line, @"\s+")
    match Int32.TryParse pair[0], Int32.TryParse pair[1] with
    | (true, i1), (true, i2) ->  (i1, i2)
    | _ -> raise (Exception "Parsing didn't work")

let list1, list2 = "input.txt" 
                   |> IO.File.ReadAllLines 
                   |> Array.map parseLine
                   |> Array.unzip
                   |> (fun (a1, a2) -> Array.sort a1, Array.sort a2 )

// Solution part 1:
let result1 = list1 
              |> Array.zip list2          
              |> Array.sumBy (fun (i1, i2) -> Math.Abs(i1 - i2))     

printfn "Problem 1: %A" result1

// Solution part 2:
let countsMap = list2 
                |> Array.countBy id
                |> Map

let result2 = list1 
              |> Array.sumBy (fun x -> 
                 match countsMap.TryGetValue(x) with
                 | (true, count) -> x * count
                 | _ -> 0)

printfn "Problem 2: %A" result2