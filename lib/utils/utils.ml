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
  let counter = ref (-1) in
  fun () ->
    let _ = counter := !counter + 1 in
    !counter

(* Terminal colours :D https://ss64.com/nt/syntax-ansi.html *)
type colour =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | LightRed
  | LightGreen
  | LightYellow
  | LightBlue
  | LightMagenta
  | LightCyan
  | White

type font = Bold | Italic | Underline
type modifier = Colour of colour | Font of font

let string_of_modifier = function
  | Font Bold -> "1"
  | Font Italic -> "3"
  | Font Underline -> "4"
  | Colour Black -> "30"
  | Colour Red -> "31"
  | Colour Green -> "32"
  | Colour Yellow -> "33"
  | Colour Blue -> "34"
  | Colour Magenta -> "35"
  | Colour Cyan -> "36"
  | Colour LightRed -> "91"
  | Colour LightGreen -> "92"
  | Colour LightYellow -> "93"
  | Colour LightBlue -> "94"
  | Colour LightMagenta -> "95"
  | Colour LightCyan -> "96"
  | Colour White -> "97"

let escape = "\027["
let reset = "\027[0m"
let escape modifier = escape ^ string_of_modifier modifier ^ "m"
let mod_string modifier str = escape modifier ^ str ^ reset
let print_endline_mod modifier str = print_endline (mod_string modifier str)
