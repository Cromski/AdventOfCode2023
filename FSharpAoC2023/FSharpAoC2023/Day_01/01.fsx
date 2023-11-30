let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let dayOne arr =
    failwith "not implemented"
    

printfn "%A" (dayOne (readLines "./FSharpAoC2023/Day_01/big.txt"))