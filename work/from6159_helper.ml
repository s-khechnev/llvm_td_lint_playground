(* This file was auto generated *)
type t = CI_hacky of string * int | CI_default of string
type info = { out : string list; inputs : string list }

module Hash_info = Hashtbl.Make (struct
  include String

  let hash = Hashtbl.hash
end)

let def ans name info = Hash_info.add ans name (CI_default name, info)

let hacky ans key name arity info =
  Hash_info.add ans key (CI_hacky (name, arity), info)

let lookup_exn hash key = Hash_info.find hash key
