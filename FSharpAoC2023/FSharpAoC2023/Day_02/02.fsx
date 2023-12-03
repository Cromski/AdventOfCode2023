open System.Text.RegularExpressions
let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let createTuple (str: string) = int(str.Split(" ")[0]),str.Split(" ")[1]
let createTupleList (lst: string list) : (int*string) list =
    List.fold (fun acc s -> (createTuple s)::acc) [] lst

let fsharpRegexIsAbsoluteGarbo (regex: string) (str: string) = // take regex and string, returns list of matches
    let rec aux (acc: string list) (s: string) =
        let m = Regex(regex).Match(s)
        if m.Success
        then aux (acc@[m.Value]) (s.Remove(s.IndexOf(m.Value),m.Value.Length))
        else acc
    aux [] str

let predicate (v:int,color:string) = 
    match color with
    | "red"   when v <= 12 -> true
    | "green" when v <= 13 -> true
    | "blue"  when v <= 14 -> true
    | _                    -> false

let checkIfGameIsPossible (str: string) =
    str
    |> fsharpRegexIsAbsoluteGarbo "\d+ [blue|red|green]+"
    |> createTupleList
    |> List.forall predicate

let ptOne (arr: string list) =
    let rec aux (acc:int) (gameNr: int) (ar: string list) =
        match ar with
        | [] -> acc
        | x :: xs -> if checkIfGameIsPossible x then aux (acc+gameNr) (gameNr+1) xs else aux acc (gameNr+1) xs
    aux 0 1 arr

let checkColorForAcc (acc1,acc2,acc3) (v:int,c:string) =
    match c with
    | "red"   when v > acc1 -> (v, acc2, acc3)
    | "green" when v > acc2 -> (acc1, v, acc3)
    | "blue"  when v > acc3 -> (acc1, acc2, v)
    | _                     -> (acc1, acc2, acc3)

let getPowerOfTriple (a,b,c) = a*b*c

let getPowerOfSet (str: string) =
    str
    |> fsharpRegexIsAbsoluteGarbo "\d+ [blue|red|green]+"
    |> createTupleList
    |> List.fold (fun (acc1,acc2,acc3) (v,c) -> checkColorForAcc (acc1,acc2,acc3) (v,c)) (0,0,0)
    |> getPowerOfTriple

let ptTwo (arr: string list) =
    arr
    |> List.fold (fun acc s -> (getPowerOfSet s)+acc) 0 

printfn "pt 1: %A" (ptOne (readLines "./Day_02/big.txt"))
printfn "pt 2: %A" (ptTwo (readLines "./Day_02/big.txt"))