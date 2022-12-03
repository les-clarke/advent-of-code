#load "common.fsx"

open System
open Common
open Common.AdventCommon
let filePath = "./inputs/inputone.txt"

let getCalorieList(filePath) =
    [
        let mutable ls = []    
        for line in (readLines filePath) do
            if String.Empty = line then
                yield ls
                ls <- []
            else
                ls <- List.append ls [line]        
    ] |> List.map (List.map int >> List.sum)

let calories = getCalorieList filePath

let max = calories |> List.max

let top = List.topN 3 calories

printfn "%d | %A | %d" max top (List.sum top)