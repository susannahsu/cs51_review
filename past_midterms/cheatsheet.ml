(* get max element in a list *)
let rec max_in_lst (lst : 'a list) : 'a =
  match lst with
  | [] -> raise (Invalid_argument "empty")
  | [hd] -> hd
  | hd :: tl ->
      max hd (max_in_lst tl) ;;