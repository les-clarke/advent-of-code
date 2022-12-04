#load "common.fsx"

open System
open Common
open Common.AdventCommon

let getPairFromString (s: string) =
    let arr = s.Split '-'
    arr[0], arr[1]

let getPairAsInt = 
    getPairFromString 
    >> (fun tup -> int (fst tup), int (snd tup))

let getAssignments (input: seq<string>) =
    input
    |> Seq.map (fun input ->
        let split = input.Split ','
        let firstPair = getPairAsInt split[0]    
        let secondPair = getPairAsInt (split[1])
        firstPair, secondPair
    )

let doPairsCompletelyOverlap ((firstPair: int * int), (secondPair: int * int)) =
    
    let doOverlap (firstPair: int * int) (secondPair: int * int) =         
        let fLeft, fRight = firstPair
        let sLeft, sRight = secondPair
        sLeft >= fLeft && fRight >= sRight
         
    doOverlap firstPair secondPair || doOverlap secondPair firstPair

// part 1
let countOverlappingPairs fn=
    Seq.map fn
    >> Seq.filter id
    >> Seq.length

let doAnyOverlappingPairs ((firstPair: int * int), (secondPair: int * int)) =
    let s1 = {fst firstPair .. snd firstPair} |> Set
    let s2 = {fst secondPair .. snd secondPair} |> Set
    let intersection = Set.intersect s1 s2
    intersection.Count = 0 |> not


let filePath: string = "./2022/inputs/inputfour.txt"
let assignments = readLines filePath |> getAssignments


printfn "%d" (countOverlappingPairs doPairsCompletelyOverlap assignments)
printfn "%d" (countOverlappingPairs doAnyOverlappingPairs assignments)