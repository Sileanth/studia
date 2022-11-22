



let rec mfold f acc xs  =
    match xs with
    | [] -> acc
    | x :: xs -> mfold f (f acc x) xs

let rec mrold f acc  xs =
    mfold f acc (List.rev xs)

List.fold

let SumAhead xs =
    let res = mfold (fun a x -> 
                match a with
                    | Some y -> Some (y + x)
                    | None -> if x < 0 then Some x else None
                ) None xs
    in match res with 
        | None -> 0
        | Some x -> x

let SumBack xs =
    let res = mrold (fun a x -> 
                match a with
                    | Some y -> Some (y + x)
                    | None -> if x < 0 then Some x else None
                ) None xs
    in match res with 
        | None -> 0
        | Some x -> x

let SumBackb xs =
    let res = List.foldBack (fun x a -> 
                match a with
                    | Some y -> Some (y + x)
                    | None -> if x < 0 then Some x else None
                ) xs None 
    in match res with 
        | None -> 0
        | Some x -> x

let SumAheadb xs =
    let res = List.fold (fun a x -> 
                match a with
                    | Some y -> Some (y + x)
                    | None -> if x < 0 then Some x else None
                ) None xs
    in match res with 
        | None -> 0
        | Some x -> x 

// rożnica między moją implementacją a biblioteczną fold back jest koljeność argumentów w lambdzie

let count xs =
    let xs = List.filter (fun x -> not (System.String.IsNullOrWhiteSpace x)) xs
    let xs = List.map (fun (x: string) -> x.Trim()) xs
    let xs = List.map (fun (x: string) -> x.Length) xs
    let l = List.length xs
    List.sum xs + (l-1)*(l)/2