
// Parsing
let orderingRules, updates  = 
    System.IO.File.ReadAllLines "input.txt"
    |> Array.partition (fun s -> s.Contains("|"))

let orderPairs = orderingRules 
                    |> Array.map (fun s -> s.Split "|")
                    |> Array.map (fun a -> (int a[0], int a[1]))
let updateArrays = updates
                   |> Array.filter (not << ((=) ""))
                   |> Array.map (fun s -> "," |> s.Split |> Array.map int)

// Helpers:
let order (i, j) = orderPairs |> Array.contains (i, j)
let isCorrectlyOrdered = 
    Array.pairwise >> Array.map order >> (Array.reduce ( && ))
let middleElement (a : int array) = a[a.Length / 2]

// Solution:
let problem1 = updateArrays 
               |> Array.filter isCorrectlyOrdered
               |> Array.map middleElement
               |> Array.sum

let problem2 = updateArrays
               |> Array.filter (not << isCorrectlyOrdered)
               |> Array.map (Array.sortWith (fun i j -> if order (j, i) then 1 else -1))
               |> Array.map middleElement
               |> Array.sum

printfn "Problem 1: %d \nProblem 2: %d" problem1 problem2