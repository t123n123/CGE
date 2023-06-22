
type card = { 
    name: string;
    hp: int;
    attack: int; 
    cost: int;
}

let string_of_card (card : card) : string = 
    (card.name ^ " - Cost: " ^ (string_of_int card.cost) ^ 
    " Stats: " ^ (string_of_int card.attack) ^ "/" ^ (string_of_int card.hp))

type playerstate = {
    hp: int;
    mana: int;
    max_mana: int;
    deck: card list; 
    hand: card list;
    board: card list;
}

let string_of_cardlist (cards : card list) : string = 
    List.fold_left (fun left right -> left ^ right) "" (List.map (fun card -> "\n" ^ string_of_card card) cards)

let string_of_playerstate( player : playerstate) : string = 
    "Player : \n" ^ 
    "HP: " ^ string_of_int player.hp ^ "\n" ^ 
    "Hand: " ^ string_of_cardlist player.hand ^ "\n\n" ^
    "Deck: " ^ string_of_cardlist player.deck ^ "\n\n" ^
    "Board: " ^ string_of_cardlist player.board ^ "\n\n"

type gamestate = {
    players : playerstate list;
}

let rec replace list index new_value = if (index == 0) 
    then ( new_value :: (List.tl list))
    else (List.hd list) :: (replace (List.tl list) (index - 1) new_value)

let draw_card (player_nr : int) (game_state : gamestate) : gamestate =
    let player = List.nth game_state.players player_nr in 
    let new_deck = List.tl player.deck in 
    let new_hand = (List.hd player.deck) :: (player.hand) in 
    {players = replace (game_state.players) player_nr {player with deck = new_deck; hand = new_hand}}  


let a = {name =  "Novice Engineer"; hp = 1; attack = 1; cost = 2} 
let game = {players = [{hp = 30; mana = 0; max_mana = 0; deck = [a]; hand = [a]; board = [a]}]}

let game2 = draw_card 0 game 

let () = print_endline (string_of_playerstate (List.hd game2.players))




