open BaseTypes

(* Event trigger checker *)

let ( &@ ) ec1 ec2 ev c = ec1 ev c && ec2 ev c
let ( |@ ) ec1 ec2 ev c = ec1 ev c || ec2 ev c
let ( !@ ) ec1 ev c = not (ec1 ev c)

let battlecry ev (c : card_instance) : bool =
  match ev with CardPlayed card -> !card.card_id == !c.card_id | _ -> false

let end_own_turn ev (c : card_instance) : bool =
  match ev with TurnEnded player -> !c.owner == player | _ -> false

let on_board _ (c : card_instance) : bool =
  Counter.find "On Board" !c.card_counters = 1

let spell_cast ev _ : bool =
  match ev with CardPlayed card -> !card.card.card_type = Spell | _ -> false

let card_type_played str ev _ : bool =
  match ev with
  | CardPlayed card -> List.exists (fun x -> x = str) !card.card.tribes
  | _ -> false

let you_spell_cast ev (c : card_instance) : bool =
  match ev with
  | CardPlayed card -> !card.card.card_type = Spell && !card.owner = !c.owner
  | _ -> false

let card_drawn ev (c : card_instance) : bool =
  match ev with CardDrawn card -> !card.owner = !c.owner | _ -> false

let played_elemental_lastturn _ (c : card_instance) : bool =
  match Counter.find_opt "Elemental Played Last Turn" !c.card_counters with
  | Some 1 -> true
  | _ -> false

(* Event Actions *)

let deal_damage dmg target =
  Instant
    (fun state ->
      match target with
      | Player x ->
          let player = List.nth state.players x in
          let new_state =
            {
              state with
              players =
                Utils.replace x state.players
                  { player with hp = player.hp - dmg };
            }
          in
          new_state
      | Card c ->
          let _ = c := { !c with card_hp = !c.card_hp - dmg } in
          state)

let do_self change_function =
  Self
    (fun card_inst ->
      Instant
        (fun x ->
          match card_inst with
          | Player _ -> x
          | Card cr ->
              let _ = change_function cr in
              x))

let set_counter (card : card_instance) counter value =
  card :=
    { !card with card_counters = Counter.add counter value !card.card_counters }

let get_counter (card : card_instance) counter =
  match Counter.find_opt counter !card.card_counters with
  | Some x -> x
  | None -> 0

let enter_board = do_self (fun cr -> set_counter cr "On Board" 1)
let played_elemental = do_self (fun cr -> set_counter cr "Elemental Played" 1)

let pass_turn_elemental =
  do_self (fun cr ->
      set_counter cr "Elemental Played Last Turn"
        (get_counter cr "Elemental Played");
      set_counter cr "Elemental Played" 0)
