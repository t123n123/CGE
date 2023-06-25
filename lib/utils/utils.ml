let replace index list new_value =
  let rec aux index list acc =
    match list with
    | [] -> List.rev acc
    | head :: tail ->
        if index == 0 then List.rev_append (new_value :: acc) tail
        else aux (index - 1) tail (head :: acc)
  in
  aux index list []

let remove (index : int) (list : 'a list) =
  let rec aux index list acc =
    match list with
    | [] -> List.rev acc
    | head :: tail ->
        if index == 0 then List.rev_append acc tail
        else aux (index - 1) tail (head :: acc)
  in
  aux index list []


let make_new_number = 
  let counter = ref 0 in 
  fun () -> let _ = (counter := !counter + 1) in !counter