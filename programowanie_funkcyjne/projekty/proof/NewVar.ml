


let counter = ref 0

let new_var () = 
  let z = !counter in 
  counter := !counter + 1;
  string_of_int z
