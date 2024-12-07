
open System.IO
open System.Text.RegularExpressions


let input = File.ReadAllText "input.txt"

let problem1 str = 
    Regex.Matches(str, @"mul\((\d+),(\d+)\)")
    |> Seq.sumBy(fun m -> (int m.Groups[1].Value) * (int m.Groups[2].Value))

printfn "Problem 1: %A" (problem1 input)

let result2 = Regex.Replace(input,  @"don't\(\)(.*)do\(\)", "")
            
let rec cleanString (startIndex : int) (str : string)  =
    let searchStr = str[startIndex..]
    let i = searchStr.IndexOf("don't()")
    if i = -1 then str 
    else
        let j = searchStr[i..].IndexOf("do()")
        if j = -1 then str[..startIndex + i] else
        cleanString (startIndex + i) (str[..startIndex + i] + str[startIndex + i + j..])
    

printfn "%A" (input |> (cleanString 0 >> problem1 ))