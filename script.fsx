let rand = System.Random(0)
let randomList size =
    seq{0..size}
    |> Seq.map (fun _ -> rand.Next(100))
    |> Seq.toList
let sortList1 = List.sort

//Quick sort
let rec sortList2 list = 
    match list with
    | [] -> []
    | pivot::rest -> 
        let left = rest |> List.filter (fun x -> x < pivot)
        let right =  rest |> List.filter (fun x -> x > pivot)
        List.concat[ (left |> sortList2) ; [pivot] ; (right |> sortList2)]

//randomList 100 |> sortList2

// Bubble sort
let sortList3 list = 
    let mutable array = list |> List.toArray
    let mutable fin = false

    while not fin do
        fin <- true
        for i = 0 to array.Length - 2 do
            if array.[i] > array.[i+1] then
                let mutable t = array.[i]
                array.[i] <- array.[i+1]
                array.[i+1] <- t
                fin <- false

    array |> Array.toList

[1;7;3;5;6;2;9;4;8]
|> sortList3
|> (printfn "%A")


