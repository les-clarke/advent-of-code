module AdventCommon = 
    open System.IO
    open System

    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let readAsString (path) = System.IO.File.ReadAllText path

    let getCharValueInOrder (c: char) = 
        let sub = if Char.IsUpper c then 38 else 96 
        (Convert.ToInt32 c) - sub

[<AutoOpen>]
module List = 
    type List<'v> with 
        static member topN<'t when 't : comparison> n (ls: 't list) =
            ls |> List.fold (fun (state: 't list) num ->
            if state.Length = 0 then
                [num]
            elif state.Length < n then
                state |> List.append [num] |> List.sortDescending
            else
                let smallest = state[n-1]
                if smallest < num then
                    List.updateAt (n-1) num state |> List.sortDescending 
                else
                    state

            ) []

[<AutoOpen>]    
module Unions = 
    open Microsoft.FSharp.Reflection
    
    let getUnionCases<'T>() =
        FSharpType.GetUnionCases typeof<'T>
        |> Seq.map (fun o ->  FSharpValue.MakeUnion(o, [||]) :?> 'T)