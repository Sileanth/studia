open FSharp.Data

let boxmuller (u1 : float) (u2 : float) =
    let n1 = sqrt (-2.0 * System.Math.Log(u1)) * sin (2.0 * System.Math.PI * u2)
    let n2 = sqrt (-2.0 * System.Math.Log(u1)) * cos (2.0 * System.Math.PI * u2)
    n1,n2

let gen_random_vars (n : int) (rand : System.Random) =
    let rec rek n = 
        match n with 
        | 0 -> []
        | x -> 
            let u1 = rand.NextDouble() in
            let u2 = rand.NextDouble() in
            let (n1, n2) = boxmuller u1 u2 in
            List.append (if (x % 2) = 1 then [n1] else [n1; n2]) (rek (n-1)) 
    rek n

let gbrownian_step (S : float) (r : float) (v : float) (t : float) (n : float) (z : float) =
    let tn = t / n
    S * System.Math.Exp((r- 0.5 * v * v) * tn + v * (sqrt tn) * z)


let calc_path (steps : int) (price : float) (drift : float) (vol : float) (years : float) (rand_vars : float list) =
    let step : float = years / (float steps)
    List.fold (fun (ys : float list) (z : float) ->  
            match ys with
                | [] -> []
                | x :: xs -> (gbrownian_step x drift vol years (float steps) z) :: ys) [price] rand_vars



let calc_vol (stocks : float list) (t : float) (n : float) =
    let ri c p =
        System.Math.Log(c * t / n / p)
    let rec rek = function
        | [] -> []
        | [_] -> []
        | p :: (c :: xs) -> ri c p :: rek (c :: xs)
    let riv = rek stocks
    let rd = List.sum riv / n
    let temp = List.map (fun (x : float) -> (x - rd) * (x - rd)) riv
    n / (t * (n - 1.0)) * List.sum temp



let calc (count : int) (steps : int) (price : float) (drift : float) (vol : float) (years : float) (seed : int) =
    let rand = System.Random(seed)
    let rec rek = function
        | 0 -> []
        | n -> gen_random_vars steps rand :: rek (n - 1)
    let xs = rek count
    let stocks = List.map (calc_path steps price drift vol years) xs
    let res = List.map (fun s -> List.head s , (calc_vol s years (float steps))) stocks
    res
    





[<EntryPoint>]
let main args =
    printfn "Arguments passed to function : %A" args
    // Return 0. This indicates success.
    0


type MyCsvType = CsvProvider<Schema = "StockPrice (float), RealizedVolatility (float)", HasHeaders = false>
   
let myCsvBuildRow (x, y: float * float) = 
  MyCsvType.Row(x, y)

let myCsvBuildTable data = 
  new MyCsvType(Seq.map myCsvBuildRow (List.toSeq data))

let myCsv = family |> myCsvBuildTable
myCsv.SaveToString()
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"




