let rand = new System.Random(0)
let randomList size =
    seq{0..size}
    |> Seq.map (fun _ -> rand.Next(100))
    |> Seq.toList
let sortList1 = List.sort

//Quick sort
let rec sortList2 list = 
    let sort2 a b = 
        match a,b with
        | a,b when a<=b -> [a;b]
        | _ -> [b;a]

    match list with
    | [] -> []
    | list -> 
        let firstValue = list |> Seq.head
        let left = list |> List.filter (fun x -> x < firstValue)
        let right =  list |> List.filter (fun x -> x > firstValue)
        List.concat[ (left |> sortList2) ; [firstValue] ; (right |> sortList2)]

[1;7;3;5;6;2;9;4;8]
|> sortList2
|> (printfn "%A")

randomList 100 |> sortList2

            