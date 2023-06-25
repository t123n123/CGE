let replace list index new_value =
  let rec aux list index new_value acc =
    match list with
    | [] -> List.rev acc
    | head :: tail ->
        if index == 0 then aux tail (index - 1) new_value (new_value :: acc)
        else aux tail (index - 1) new_value (head :: acc)
  in
  aux list index new_value []

let rec remove list index =
  if index == 0 then List.tl list
  else List.hd list :: remove (List.tl list) (index - 1)
