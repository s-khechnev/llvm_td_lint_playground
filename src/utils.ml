let chop_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

let rm_duplicates list =
  let rec helper acc = function
    | [] -> acc
    | x :: xs -> if List.mem x acc then helper acc xs else helper (x :: acc) xs
  in
  helper [] list

let failwithf fmt = Format.kasprintf failwith fmt
let printfn ppf = Format.kasprintf print_endline ppf
let debug = false
let log fmt = if debug then Format.kasprintf print_endline fmt
