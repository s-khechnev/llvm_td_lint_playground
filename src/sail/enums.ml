(* This file was auto generated *)

let enums =
  let ans = Hashtbl.create 150 in
  Hashtbl.add ans "AnEnum" ["Three"; "Two"; "One"];
  ans
   
let find_opt = Hashtbl.find_opt enums
let find_exn = Hashtbl.find enums
let mem = Hashtbl.mem enums 
