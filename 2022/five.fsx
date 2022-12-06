#load "common.fsx"

open System
open System.Text.RegularExpressions
open System.Text
open Common
open Common.AdventCommon


let chunk n (s: string) =         
    s.ToCharArray()
    |> Seq.filter (fun c -> not (c = '\n'))
    |> Seq.chunkBySize n
    |> Seq.map System.String
    |> Seq.toList

let hasAlpha(s: string) = 
    s.ToCharArray()
    |> Array.fold (fun state c -> state || System.Char.IsLetter(c)) false


let expr = Regex("^[0-9]*$")
let hasOnlyNum(s: string) = 
    s <> null && not (s = "") && expr.IsMatch(s)

let parseInput (lines: string seq ) = 
    let mutable firstLength = 0
    let mutable stacks = 0
    let mutable stackputs: string list = []
    let mutable instructions = []
    
    lines 
    |> Seq.iter (fun s ->
        let containsMove = s.Contains("move")        
        let spaceless = s.ToCharArray() |> Array.filter (fun c -> not (c = ' ')) |> System.String        
        let spacelessIsNums = hasOnlyNum spaceless

        let len = s.Length
        if len > 0 && firstLength = 0 then
            firstLength <- len
            printfn "%d" firstLength            
            stackputs <- stackputs @ [s]        
        elif firstLength > 0 && len = firstLength && hasAlpha(s) then
            stackputs <- stackputs @ [s]
        elif spacelessIsNums then 
            let all = s.Split(' ') |> Array.toList |> List.filter (fun s -> not (s = ""))
            
            let rec last (ls: string list) = 
                match ls with 
                | [] -> failwith "asdasdf"
                | [x] -> x
                | head::tail -> last tail
            
            let tail = last all            
            stacks <- int tail

            ()
        elif containsMove then
            let nums = 
                let split = s.Split(' ')                
                split
                |> Array.filter (hasOnlyNum)
                |> Array.map int
            
            instructions <- instructions @ [nums[0], nums[1] - 1, nums[2] - 1]
    )

    let arr: char list array = seq {for _ in 0..stacks - 1 do yield []} |> Array.ofSeq
    printfn "stacks: %d| %d" stacks arr.Length
    for s in stackputs do
        let chunked = chunk 4 s
        printfn "chunked length %d. S %d length" chunked.Length s.Length
        chunked
        |> List.iteri (fun i s ->
            printfn "%d" i
            let maybeC = s.ToCharArray() |> Array.tryFind (fun c -> System.Char.IsLetter(c))
            if maybeC.IsSome then 
                if i >= arr.Length then 
                    printfn "err %s|%c"  s maybeC.Value

                let ls = arr[i] @ [maybeC.Value]
                arr[i] <- ls            
        )    
    
    arr, instructions

let printArr (arr: char list array) =
    printfn "printing arr"
    let rec padList (max: int) (ls: char list) = 
        if ls.Length = max then 
            ls
        else
            let ls2 = [' '] @ ls
            padList max ls2

    let sb = StringBuilder()
    let max = arr |> Array.map (fun ls -> ls.Length) |> Array.max
    let final = 
        arr
        |> Array.map (fun ls -> 
            if ls.Length < max then 
                padList max ls
            else ls
        )
    let mutable current = 0
    while current < max do
        for ls in final do 
            sb.Append(ls[current]) |> ignore
            sb.Append(' ') |> ignore
        sb.Append('\n') |> ignore
        current <- current + 1

    printfn "%s" (sb.ToString())


let applyInstructions (arr: char list array, instructions: (int * int * int) list) =
    for i in instructions do
        // printArr arr
        let move, from, to' = i
        let stack = arr[from]
        
        let rec pop ls acc move = 
            if move = 0 then 
                ls, acc
            else
                match ls with 
                | [] -> failwith "er"
                | head::tail ->                    
                    let acc2 = acc @ [head]
                    pop tail acc2 (move - 1)
        
        let remainder, removed = pop stack [] move
        let pushTo = arr[to']
        let rec push ps ls = 
            match ps with 
            | [] -> ls
            | head::tail ->
                let newLs = [head] @ ls
                push tail newLs
        let finalTo = push removed pushTo
        arr[from] <- remainder
        arr[to'] <- finalTo
    arr

let applyInstructions2 (arr: char list array, instructions: (int * int * int) list) =
    for i in instructions do
        // printArr arr
        let move, from, to' = i
        let stack = arr[from]
        
        let rec pop ls acc move = 
            if move = 0 then 
                ls, acc
            else
                match ls with 
                | [] -> ls, acc
                | head::tail ->                    
                    let acc2 = acc @ [head]
                    pop tail acc2 (move - 1)
        
        let remainder, removed = pop stack [] move
        // let remainder = stack |> List.take
        let pushTo = arr[to']

        let finalTo = removed @ pushTo
        arr[from] <- remainder
        arr[to'] <- finalTo
    arr

// let input = 
//     """
//         [D]    
//     [N] [C]    
//     [Z] [M] [P]
//     1   2   3 

//     move 1 from 2 to 1
//     move 3 from 1 to 3
//     move 2 from 2 to 1
//     move 1 from 1 to 2
//     """ |> fun s -> s.Split('\n')

let filePath: string = "./2022/inputs/inputfive.txt"
let input = readLines filePath 

let y   = 
    parseInput input
    // |> applyInstructions

// let arr1 = applyInstructions y
let arr2 = applyInstructions2 y


for arr in [ arr2] do
    let sb = StringBuilder()
    for x in arr do 
        if x.Length > 0 then
            sb.Append(x.Head) |> ignore

    printfn "%s" (sb.ToString())
