#load "common.fsx"

open System
open System.Text.RegularExpressions
open System.Text
open Common
open Common.AdventCommon

let addCharToWindow (arr: char array) (c: char) = 
    let s = seq {0 .. arr.Length - 2}
    s |> Seq.iter (fun i -> 
        arr[i] <- arr[i+1]        
    )
    arr[arr.Length - 1] <- c
    arr

let isUniqueSequence (arr: char array) = 
    set(arr).Count = arr.Length

let detectStateOfSequence (uniqueSeqLen: int) (s: string) = 
    let arr: char array = Array.create uniqueSeqLen ' '
    let chars = s.ToCharArray() |> Array.toList

    let rec findSeq (chars: char list) (arr: char array) (calls: int) =
        match chars with
        | [] -> calls
        | head::tail ->
            let arr = addCharToWindow arr head
            if calls > 3 && isUniqueSequence arr then 
                calls + 1
            else
                findSeq tail arr (calls + 1)
    
    findSeq chars arr 0

readAsString "./2022/inputs/inputsix.txt"
|> detectStateOfSequence 14


    