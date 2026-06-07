open Effect
open Effect.Deep

type action = game -> game

and game = {
  number : int;
  stack : (unit -> action) list;
  step : (unit -> action) -> unit -> action;
}

type _ Effect.t += PlayCard : action -> action t
type _ Effect.t += Action : action -> action t

let null act =
 fun () -> try act () with effect Action _, k -> continue k (fun n -> n)

let on_play result act =
 fun () ->
  try act ()
  with effect PlayCard f, k ->
    let first = result () in
    let second = continue k (fun n -> f n) in
    fun game -> second (first game)

let play_card f () =
  let remove_itself_from_stack game =
    { game with stack = List.tl game.stack }
  in
  let do_card_effect = perform (PlayCard f) in
  fun game -> do_card_effect (remove_itself_from_stack game)

let play_null_card = play_card (fun game -> game)
let incr game = { game with number = game.number + 1 }

let incr_on_play game =
  { game with step = (fun act -> game.step (on_play (fun () -> incr) act)) }

let play_incr_on_play = play_card (fun game -> incr_on_play game)

let just act =
 fun () ->
  try act () with
  | effect PlayCard f, k -> continue k (fun n -> f n)
  | effect Action f, k -> continue k (fun n -> f n)

let new_game = { number = 0; stack = []; step = just }

let run_game game =
  if List.is_empty game.stack then game
  else
    let f = game.step (List.hd game.stack) in
    f () game

(* let () =
  let game0 = { new_game with stack = [ play_null_card ] } in
  let game1 = run_game game0 in
  print_int game1.number *)

let () =
  let game0 = new_game in
  let game1 =
    { game0 with stack = [ play_incr_on_play; play_null_card; play_null_card ] }
  in
  let game2 = run_game game1 in
  let game3 = run_game game2 in
  let game4 = run_game game3 in
  print_int game2.number;
  print_string "--";
  print_int (List.length game2.stack);
  print_newline ();
  print_int game3.number;
  print_string "--";
  print_int (List.length game3.stack);
  print_newline ();
  print_int game4.number;
  print_string "--";
  print_int (List.length game4.stack)
