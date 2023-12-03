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
    
let getFullNumberInStringFromCoord ((x,y):Coord) (matrix: string list) =
    if x < 0 || y < 0 || y >= matrix.Length || x >= matrix[y].Length then "" else
    if not (Char.IsDigit (matrix[y][x])) then "" else
    let rec aux1 acc x (str: string) =
        match x with
        | x when str = "" || x < 0 || not (Char.IsDigit str[x]) -> acc
        | x when Char.IsDigit (str[x]) -> aux1 (string (str[x])+acc) (x-1) str
        | _ -> failwith "never"
        
    let rec aux2 acc x (str: string) =
        match x with
        | x when str = "" || x >= str.Length || not (Char.IsDigit str[x]) -> acc
        | x when Char.IsDigit (str[x]) -> aux2 (acc+string (str[x])) (x+1) str
        | _ -> failwith "never"
    aux1 "" (matrix[y].Substring(0,x).Length-1) (matrix[y].Substring(0,x))
    +
    aux2 "" 0 (matrix[y].Substring(x,matrix[y].Length-x))

let rec findNextNonDigit (str: string) (x: int) =
    let sub = str.Substring(x,str.Length-x)
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
        | s when returnNumberIfAdjacentSymbol (x,row) matrix <> "." -> aux (acc@[getFullNumberInStringFromCoord (x,row) matrix]) s (x+(findNextNonDigit s x))
        | _ -> aux acc s (x+1)
    (aux [] str 0) |> List.map int |> List.sum
    
let ptOne (arr: string list) =
    let rec aux acc row ar =
        match ar with
        | [] -> acc
        | x :: xs -> aux (acc+(getIntListOfString x row arr)) (row+1) xs
    aux 0 0 arr

let findStarsInString (str: string) (row: int) =
    let rec aux acc sub (add: int) =
        match sub with
        | sub when sub = "" || sub.IndexOf("*") < 0 -> acc
        | sub -> aux (acc@[(sub.IndexOf("*")+add,row)]) (sub.Substring(sub.IndexOf("*")+1)) (add+sub.IndexOf("*")+1)
    aux [] str 0

let getNumbersAroundStar ((x,y):Coord) (matrix: string list) =
    [
     getFullNumberInStringFromCoord (x-1,y-1) matrix; getFullNumberInStringFromCoord (x,y-1) matrix
     getFullNumberInStringFromCoord (x+1,y-1) matrix; getFullNumberInStringFromCoord (x-1,y) matrix
     getFullNumberInStringFromCoord (x+1,y) matrix; getFullNumberInStringFromCoord (x-1,y+1) matrix
     getFullNumberInStringFromCoord (x,y+1) matrix; getFullNumberInStringFromCoord (x+1,y+1) matrix
     ]

let getNumbersAboutLotsOfStars (matrix: string list) ((x): Coord list) =
    List.fold (fun acc (x,y) -> (getNumbersAroundStar (x,y) matrix)::acc) [] x

let fixList (lst: string list) =
    let rec aux acc (lst: string list) =
        match lst with
        | [] -> acc
        | x :: y :: xs when x <> y -> aux (acc@[x;""]) (y::xs)
        | _ :: y :: xs -> aux acc (y::xs)
        | x :: xs -> aux (x::acc) xs
    (aux [] lst) |> List.filter (fun s -> s <> "")
    
let fixLotsOfLists (lst: string list list) = 
    List.fold (fun acc l -> fixList l::acc) [] lst

let getGearPower (lst: string list) =
    if lst.Length <> 2 then 0 else
    lst
    |> List.map int
    |> List.fold (fun acc s -> s*acc) 1

let ptTwo (arr: string list) =
    let rec aux acc l (row: int) =
        match l with
        | [] -> acc
        | x :: xs -> aux (acc@[findStarsInString x row]) xs (row+1)
    (aux [] arr 0)
    |> List.filter (fun l -> l <> [])
    |> List.collect id
    |> getNumbersAboutLotsOfStars arr
    |> fixLotsOfLists
    |> List.fold (fun acc a -> getGearPower a+acc) 0

printfn "pt 1: %A" (ptOne (readLines "./Day_03/big.txt"))
printfn "pt 2: %A" (ptTwo (readLines "./Day_03/big.txt"))