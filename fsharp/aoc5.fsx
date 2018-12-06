open System

let testFileName = "inputs/input-5.0.txt"
let fileName = "inputs/input-5.1.txt"

let readLines filePath = System.IO.File.ReadLines(filePath)
let testInput = Seq.head (readLines testFileName)
let input = Seq.head (readLines fileName)

let ucase = System.Char.ToUpper
let sameLetter a b = (ucase a) = (ucase b)
let sameLetterDiffPolarity a b = (sameLetter a b) && not (a = b)

let reactionAccumulator acc elem = 
    match acc with
       | [] -> elem :: []
       | h :: t -> if sameLetterDiffPolarity h elem then t else elem :: acc

let reactPolymer polymer = 
    let c = Seq.toList polymer in
        Seq.fold reactionAccumulator [] c
        |> List.rev
        |> Array.ofList
        |> System.String.Concat

let partone () = 
    reactPolymer input
    |> String.length