open System

let testFileName = "inputs/input-5.0.txt"
let fileName = "inputs/input-5.1.txt"

let readLines filePath = System.IO.File.ReadLines(filePath)
let testInput = Seq.head (readLines testFileName)
let input = Seq.head (readLines fileName)

let ucase = System.Char.ToUpper
let lcase = System.Char.ToLower

let bothCase a = (ucase a) :: (lcase a) :: []

let sameLetter a b = (ucase a) = (ucase b)
let sameLetterDiffPolarity a b = (sameLetter a b) && not (a = b)

let isNotLetter a =
    let u :: l :: r = bothCase a in
        (fun x -> (not (x = u)) && (not (x = l)))

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

let parttwo input =
    let inputList = Seq.toList input in
    let letters = Seq.map (fun x -> char x) (seq {65 .. (65 + 25)}) |> Seq.toList in
        let filteredInputs = List.map (fun letter -> List.filter (isNotLetter letter) inputList) letters in
            List.map reactPolymer filteredInputs
            |> List.map String.length
            |> Seq.min

let Value (c:char) = 
    (int c) - (int 'A') + 1