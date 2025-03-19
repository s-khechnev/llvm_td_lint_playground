let chop_suffix ~suffix s =
  if String.ends_with ~suffix s then
    String.sub s 0 (String.length s - String.length suffix)
  else s

let failwithf fmt = Format.kasprintf failwith fmt
let printfn ppf = Format.kasprintf print_endline ppf
let debug = false
let log fmt = if debug then Format.kasprintf print_endline fmt
