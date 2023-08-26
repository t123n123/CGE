open BaseTypes
open Card

let ( << ) f g x = f (g x)

let rec ( <*> ) (a1 : action) (a2 : action) : action =
  match (a1, a2) with
  | Instant f1, Instant f2 -> Instant (f1 << f2)
  | Targetted f1, _ -> Targetted (fun action -> f1 action <*> a2)
  | _, Targetted f2 -> Targetted (fun action -> a1 <*> f2 action)
  | Self f1, _ -> Self (fun action -> f1 action <*> a2)
  (* This only happens when first is Instant *)
  | _, Self f2 -> Self (fun action -> a1 <*> f2 action)

let get_all_targets state =
  List.map (fun x -> (x.player_id, Player x.player_id)) state.players
  @ List.concat
  @@ List.map
       (fun x -> List.map (fun card -> (!card.card_id, Card card)) x.board)
       state.players

let show_target target =
  match target with
  | Card x -> string_of_card x
  | Player x -> "Player: " ^ string_of_int x

let get_target state =
  let targets = get_all_targets state in
  let _ =
    List.map
      (fun (id, target) ->
        let _ = print_string (string_of_int id ^ " -> ") in
        print_endline @@ show_target target)
      targets
  in
  let number = read_int () in
  let _, tar = List.find (fun (id, _) -> number = id) targets in
  tar

let rec apply_action action state =
  match (action : action) with
  | Instant f -> f state
  | Targetted f -> apply_action (f @@ get_target state) state
  | Self _ -> state
