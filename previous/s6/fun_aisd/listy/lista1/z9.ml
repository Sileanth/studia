

let rec suffixes xs = 
  match xs with
  | [] -> [[]]
  | y :: ys -> xs :: suffixes ys




