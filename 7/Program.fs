
open System.Text.RegularExpressions
let input = "testdata.txt" 
            |> System.IO.File.ReadAllLines 
            |> Array.map (fun s -> s.Split ":")
            |> Array.map (fun a -> int a[0], Array.map int (Regex.Split(a[1], @"\s+"))) 

Set.