
let input = "input.txt" |> System.IO.File.ReadAllLines
                      |> Array.map (fun s -> s.ToCharArray())    
let theMap = array2D input             

type Direction = N | E | S | W
let nextDir = 
    function | N -> E | E -> S | S -> W | W -> N
let visitedStraight (i, j) direction steps = 
    match direction with 
    | N -> [ for k in i .. -1 .. i - steps -> (k, j) ]
    | S -> [ for k in i .. i + steps -> (k, j) ]
    | W -> [ for k in j .. -1 .. j - steps -> (i, k) ]
    | E -> [ for k in j .. j + steps -> (i, k) ]

let rec visitedPositions (i, j) visited direction =
    let line = match direction with
               | N -> [| for k in i .. -1 .. 0 -> theMap[k, j] |]
               | S -> theMap[i.., j]
               | W -> [| for k in j .. -1 .. 0 -> theMap[i, k] |]
               | E -> theMap[i, j..]

    let steps, proceed = 
        try
            (line |> Array.findIndex ((=)'#')) - 1, true
        with
            _ -> (line |> Array.length) - 1, false

    let newVisited = visitedStraight (i,j) direction steps
    let visitedSoFar = Set.union visited (set newVisited)

    if proceed
    then 
        let newPos = newVisited |> List.last
        let newDir = nextDir direction
        visitedPositions newPos visitedSoFar newDir
    else 
        visitedSoFar

let i = input |> Array.findIndex (Array.exists ((=) '^'))
let j = input[i] |> Array.findIndex ((=) '^')
let all = visitedPositions (i, j) (set [(i, j)]) N

printfn "%A" (Set.count all)