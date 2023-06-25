type card = {
  name : string;
  health : int;
  attack : int;
  cost : int;
  card_type : CardType.t;
  triggers : trigger list;
}

and card_instance = { card : card; owner : int; card_id : int }
and trigger = (event -> card_instance -> bool) * action

and event =
  | CardPlayed of card_instance
  | CardDeath of card_instance
  | CardDrawn of card_instance

and target = Card of card_instance | Player of int

and action =
  | Instant of (gamestate -> gamestate)
  | Targetted of (target -> action)

and playerstate = {
  hp : int;
  mana : int;
  max_mana : int;
  player_id : int;
  deck : card_instance list;
  hand : card_instance list;
  board : card_instance list;
}

and gamestate = { players : playerstate list; current_player : int }
