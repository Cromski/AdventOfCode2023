let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let dayOne (arr: string list) : string list =
    failwith "not implemented"
    

printfn "%A" (dayOne (readLines "./Day_01/big.txt"))