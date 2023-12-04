open System.Text.RegularExpressions
let readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let fsharpRegexIsAbsoluteGarbo (regex: string) (str: string) = // take regex and string, returns list of matches
    let rec aux (acc: string list) (s: string) =
        let m = Regex(regex).Match(s)
        if m.Success
        then aux (acc@[m.Value]) (s.Remove(s.IndexOf(m.Value),m.Value.Length))
        else acc
    aux [] str

let bingo (lst1: string list) (lst2: string list) _ =
    (List.fold (fun acc x -> if List.contains x lst2 then acc*2 else acc) 1 lst1) / 2

let getPointsForOneGame f (str: string) (big: bool) (i: int) =
    let numbers = str
                  |> fsharpRegexIsAbsoluteGarbo "\d+"
    if big
        then
            f (List.init 10 (fun x -> numbers[x+1])) (List.init 25 (fun x -> numbers[x+11])) i
        else
            f (List.init 5 (fun x -> numbers[x+1])) (List.init 8 (fun x -> numbers[x+6])) i
// getPointsForOneGame bingo "Card   1: 33 13 28 76 16 91 52 41 38 64 | 52 10  7 61 12 70 84 38 16 40  5 49 33 11 31 43 71 28 72 23 98 47 14 44 90" true

let ptOne (arr: string list) =
    failwith "not implemeneted"
    arr
    |> List.fold (fun acc s -> getPointsForOneGame bingo s true 0+acc) 0 

let mutable gameIds = List.init (readLines "./Day_04/mini.txt").Length (fun i -> i)

let insertInGameIds (lst: int list) =
    gameIds <- (List.sort (lst@gameIds))

let bango (lst1: string list) (lst2: string list) (a: int) =
    List.init (List.fold (fun acc x -> if List.contains x lst2 then acc+1 else acc) 0 lst1) (fun i -> i+a)

let getCardId (card: string) =
    card
    |> fsharpRegexIsAbsoluteGarbo "\d+"
    |> List.head
    |> int

// let s = "Card 209: 81 45 40 33 88 41 95 89 15 51 | 31 41 14 55 76 58 23 73 12 10 69 17 61 71  6 21 82  8 20 57 42 66 95 37 72"
// getPointsForOneGame bango s true (getCardId s)
// getPointsForOneGame bango "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19" false (getCardId "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")

let ptTwo (arr: string list) =
    let mutable x = 0
    while x < gameIds.Length-1 do
    insertInGameIds (getPointsForOneGame bango (arr[gameIds[x]]) false (getCardId arr[gameIds[x]]))
    x <- x+1
    

printfn "pt 2: %A" (ptTwo (readLines "./Day_04/mini.txt"))
printfn "gameIds: %A" gameIds.Length

printfn "pt 1: %A" (ptOne (readLines "./Day_03/mini.txt"))
printfn "pt 1: %A" (ptOne (readLines "./Day_04/big.txt"))