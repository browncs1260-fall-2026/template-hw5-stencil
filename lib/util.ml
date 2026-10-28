let gensym : string -> string =
  let counter = ref 0 in
  fun (base : string) ->
    let number = !counter in
    counter := !counter + 1 ;
    Printf.sprintf "%s__%d" base number

module List = struct
  include List

  let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi
end

let rec input_all (ch : in_channel) : string =
  try
    let c = input_char ch in
    String.make 1 c ^ input_all ch
  with End_of_file -> ""
