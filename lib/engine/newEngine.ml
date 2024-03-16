(* Engine Stuff *)
type card = {
  name : string;
  attributes : card_attribute list;
  modifiers : card_modifier list;
}

and card_data = {
  card : card;
  owner : player_instance;
}

and card_instance = card_data ref 

and player_instance = player_data ref 

and player_data = {
  player_name : string;
  resources : player_resources; 
  player_card_zones : card_zones;
}

and game_state = {
  players : player_instance list;
  event_triggers : event_trigger list;
  trigger_stack : event Stack.t;
}

and event_trigger = (game_state -> event -> game_state)  


(* Game Stuff *)
and card_attribute = 
  | Health of int 
  | Attack of int 
  | Cost of int 

and counter = 
  | Dormant of int 
  | Infuse of int 

and tribe = 
  | Beast 
  | Mech 
  | Elemental 

and player_resources = { 
  mana : int; 
  max_mana : int; 
  health : int;
  max_health : int;
}

and card_modifier = 
  | HealthBuff of int 
  | AttackBuff of int 
  | Counter of counter 
  | Tribe of tribe
  | Stealthed 
  | DivineShield 
  | SummonFatigue
  | Freeze

and card_zones = { 
  hand : card_instance list; 
  deck : card_instance list;
}

and event = 
  | GameStartEvent 
  | PlayerDamage of int * player_instance 
  | TurnStart 

(* Engine Functions *)
let resolve (game_state : game_state) = 
  let event = Stack.pop game_state.trigger_stack in 
  let game_state = List.fold_left (fun x y -> y x event) game_state game_state.event_triggers in  
  game_state

let add_event_trigger (game_state : game_state) (event_trigger : event_trigger) = 
  { game_state with event_triggers = event_trigger::game_state.event_triggers }

let add_event (game_state : game_state) (event : event) = 
  Stack.push event game_state.trigger_stack;
  game_state

let create_card (name : string) (attributes : card_attribute list) (modifiers : card_modifier list) (owner : player_instance) : card_instance = 
  ref { card = { name = name; attributes = attributes; modifiers = modifiers }; owner = owner }

let create_player (name : string) : player_instance = 
  ref { player_name = name; resources = { mana = 0; max_mana = 0; health = 30; max_health = 30 }; player_card_zones = { hand = []; deck = [] } }

let create_game (players : player_instance list) (event_triggers : event_trigger list) : game_state = 
  { players = players; event_triggers = event_triggers; trigger_stack = Stack.create () }


(* *)

let draw_card (player : player_instance) = 
  match !player.player_card_zones.deck with 
  | [] -> player 
  | h::t -> 
    let player = { !player with player_card_zones = { !player.player_card_zones with deck = t } } in 
    let player = { player with player_card_zones = { player.player_card_zones with hand = h::player.player_card_zones.hand } } in 
    ref player


let turnStartEventTrigger = 
  fun game_state event -> 
    match event with 
    | TurnStart -> 
      let player = List.hd game_state.players in 
      let player = draw_card player in 
      let game_state = { game_state with players = player::(List.tl game_state.players) } in 
      game_state
    | _ -> game_state

let gameStartEventTrigger = 
  fun game_state event ->
    match event with 
    | GameStartEvent -> 
      let game_state = add_event game_state TurnStart in 
      game_state
    | _ -> game_state

let playerDamageEventTrigger = 
  fun game_state event -> 
    match event with 
    | PlayerDamage (damage, player) -> 
      let player = { !player with resources = { !player.resources with health = !player.resources.health - damage } } in 
      let game_state = { game_state with players = (ref player)::(List.tl game_state.players) } in 
      game_state
    | _ -> game_state


let addPlayer (game_state : game_state) (player : player_instance) = 
  { game_state with players = player::game_state.players }

let addCardToDeck (player : player_instance) (card : card_instance) = 
  let player = { !player with player_card_zones = { !player.player_card_zones with deck = card::!player.player_card_zones.deck } } in 
  player

