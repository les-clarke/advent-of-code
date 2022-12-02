#load "common.fsx"

open System
open Common
open Common.AdventCommon
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Collections


type GameResult = 
| Win 
| Loss 
| Draw
with
    static member Score = function
        | Win -> 6
        | Loss -> 0
        | Draw -> 3

    static member FromInput (c: char) =
        match System.Char.ToUpper c with 
        | 'X' -> Loss
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith "invalid input"

type Move = 
    | Rock
    | Paper
    | Scissors
with 
    static member FromInput(c: char) =
        match System.Char.ToUpper c with 
        | 'A'
        | 'X' -> Rock
        | 'B'
        | 'Y' -> Paper
        | 'C'
        | 'Z' -> Scissors
        | _ -> failwith "invalid input"
    static member Score = function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    static member GetResultForPlayer (opponent: Move) (player: Move) =
        if opponent = player then 
            Draw
        elif (opponent = Rock && player = Scissors) || (opponent = Paper && player = Rock) || (opponent = Scissors && player = Paper) then 
            Loss
        else 
            Win
    
    static member Moves = getUnionCases<Move>()
    
    static member GetMoveFromResult (opponent: Move) (result: GameResult) =                 
        Move.Moves
        |> Seq.map (fun move -> 
            ((Move.GetResultForPlayer opponent move) = result), move
        )
        |> Seq.find fst
        |> snd

        

// Part 1
// readLines "./inputs/inputtwo.txt"
// |> Seq.map (fun inputStr -> 
//     let chars = Seq.toArray inputStr
//     if chars.Length > 0 then 
//         let opponent = Move.FromInput chars[0]
//         let player = Move.FromInput chars[chars.Length - 1]
//         let result = Move.GetResultForPlayer opponent player
//         (Move.Score player) + (GameResult.Score result)
//     else 
//         0
// )
// |> Seq.sum
    
readLines "./inputs/inputtwo.txt"
|> Seq.map (fun inputStr -> 
    let chars = Seq.toArray inputStr
    if chars.Length > 0 then 
        let opponent = Move.FromInput chars[0]
        let desiredResult = GameResult.FromInput chars[chars.Length - 1]
        let playerMove = Move.GetMoveFromResult opponent desiredResult        
        (Move.Score playerMove) + (GameResult.Score desiredResult)
    else 
        0
)
|> Seq.sum
