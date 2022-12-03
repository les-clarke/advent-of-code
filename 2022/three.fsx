#load "common.fsx"

open System
open Common
open Common.AdventCommon
let filePath = "./2022/inputs/inputthree.txt"


let input = readLines filePath
input
|> Seq.map (fun (s: string) ->
    let mid = s.Length / 2
    let left = s.Substring(0, mid)
    let right = s.Substring(mid)

    let ls = Set(left)
    let rs = Set(right)
    let intersect = Set.intersect ls rs
    if intersect.Count > 1 then 
        failwith "oops"
    let char = intersect.MaximumElement
    getCharValueInOrder char

)
|> Seq.sum

input
|> Seq.chunkBySize 3
|> Seq.map (fun s ->
    s 
    |> Array.map set
    |> Set.intersectMany
    |> fun st ->
        if st.Count > 1 then 
            failwith "woops"
        getCharValueInOrder st.MaximumElement
)
|> Seq.sum