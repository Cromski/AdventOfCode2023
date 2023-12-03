open System
let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

type Coord = int*int

let isCoordASymbol ((x,y):Coord) (matrix: string list) = 
    if x < 0 || y < 0 || y >= matrix.Length || x >= matrix[y].Length then false else
        match matrix[y][x] with
        | c when Char.IsDigit c -> false
        | c when c = char "."   -> false
        | _                     -> true

let checkAbove ((x,y):Coord) (matrix: string list) : bool =
    isCoordASymbol (x-1,y-1) matrix || isCoordASymbol (x,y-1) matrix || isCoordASymbol (x+1,y-1) matrix

let checkBelow ((x,y):Coord) (matrix: string list) : bool =
    isCoordASymbol (x-1,y+1) matrix || isCoordASymbol (x,y+1) matrix || isCoordASymbol (x+1,y+1) matrix

let checkLeft ((x,y):Coord) (matrix: string list) : bool =
    isCoordASymbol (x-1,y) matrix
    
let checkRight ((x,y):Coord) (matrix: string list) : bool =
    isCoordASymbol (x+1,y) matrix

let returnNumberIfAdjacentSymbol ((x,y):Coord) (matrix: string list) =
    if checkAbove (x,y) matrix || checkBelow (x,y) matrix || checkLeft (x,y) matrix || checkRight (x,y) matrix
    then string (matrix[y][x])
    else "."
    
let getFullNumberInStringFromCoord (str: string) ((x,y):Coord) (matrix: string list) =
    if not (Char.IsDigit (matrix[y][x])) then "" else
    let rec aux1 acc x (str: string) =
        match x with
        | x when str = "" || x < 0 || not (Char.IsDigit str[x]) -> acc
        | x when Char.IsDigit (str[x]) -> aux1 (string (str[x])+acc) (x-1) str
        | _ -> failwith "unforeseen"
        
    let rec aux2 acc x (str: string) =
        match x with
        | x when str = "" || x >= str.Length || not (Char.IsDigit str[x]) -> acc
        | x when Char.IsDigit (str[x]) -> aux2 (acc+string (str[x])) (x+1) str
        | _ -> failwith "unforeseen"
    aux1 "" (str.Substring(0,x).Length-1) (str.Substring(0,x))
    +
    aux2 "" 0 (str.Substring(x,str.Length-x))

let rec findNextNonDigit (str: string) (x: int) =
    let sub = str.Substring(x,str.Length-x)
    // printfn "%A" sub
    let rec aux acc (s: string) =
        match s with
        | s when s = "" -> acc
        | s when not (Char.IsDigit s[0]) -> acc
        | s ->
            aux (acc+1) (s.Substring(1,s.Length-1))
    aux 0 sub
    
let getIntListOfString (str: string) (row: int) (matrix: string list) =
    let rec aux acc (s: string) (x: int) =
        match s with
        | s when x >= s.Length -> acc
        | s when Char.IsDigit s[x] = false -> aux acc s (x+1)
        | s when returnNumberIfAdjacentSymbol (x,row) matrix <> "." -> aux (acc@[getFullNumberInStringFromCoord s (x,row) matrix]) s (x+(findNextNonDigit s x))
        | _ -> aux acc s (x+1)
    (aux [] str 0) |> List.map int |> List.sum
    
let ptOne (arr: string list) =
    let rec aux acc row ar =
        match ar with
        | [] -> acc
        | x :: xs -> aux (acc+(getIntListOfString x row arr)) (row+1) xs
    aux 0 0 arr

printfn "pt 1: %A" (ptOne (readLines "./Day_03/big.txt"))
