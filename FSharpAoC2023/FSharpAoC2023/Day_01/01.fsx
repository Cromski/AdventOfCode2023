let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let dayOne arr =
    failwith "not implemented"
    

printfn "%A" (dayOne (readLines "./Day_01/big.txt"))