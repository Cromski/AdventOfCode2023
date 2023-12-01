open System
let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let addStringAtIndex (s: string) (i: int) (c: string) : string =
    s[0..i]+c+s[i+1..s.Length]

let rec fixString (s: string) : string =
    match s with
    | s when s.Contains "one" -> fixString (addStringAtIndex s (s.IndexOf "one") "1")
    | s when s.Contains "two" -> fixString (addStringAtIndex s (s.IndexOf "two") "2")
    | s when s.Contains "three" -> fixString (addStringAtIndex s (s.IndexOf "three") "3")
    | s when s.Contains "four" -> fixString (addStringAtIndex s (s.IndexOf "four") "4")
    | s when s.Contains "five" -> fixString (addStringAtIndex s (s.IndexOf "five") "5")
    | s when s.Contains "six" -> fixString (addStringAtIndex s (s.IndexOf "six") "6")
    | s when s.Contains "seven" -> fixString (addStringAtIndex s (s.IndexOf "seven") "7")
    | s when s.Contains "eight" -> fixString (addStringAtIndex s (s.IndexOf "eight") "8")
    | s when s.Contains "nine" -> fixString (addStringAtIndex s (s.IndexOf "nine") "9")
    | s -> s

let ptOne (arr: string list) =
    arr
    |> List.map (fun s -> String.filter Char.IsDigit s) //remove letters
    |> List.map (fun s -> s[0..0]+s[s.Length-1..s.Length]) //get first and last digit
    |> Seq.sumBy int //get sum of ints

let ptTwo (arr: string list) =
    arr
    |> List.map (fun s -> fixString s) //fixes these dumb strings :)
    |> ptOne
    

printfn "pt 1: %A" (ptOne (readLines "./Day_01/big.txt"))
printfn "pt 2: %A" (ptTwo (readLines "./Day_01/big.txt"))