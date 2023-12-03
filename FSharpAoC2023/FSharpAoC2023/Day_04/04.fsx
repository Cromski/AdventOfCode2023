let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let ptOne (arr: string list) =
    failwith "not implemeneted"

printfn "pt 1: %A" (ptOne (readLines "./Day_03/mini.txt"))
