open Effect
open Effect.Deep

type action = int -> int
type _ Effect.t += Action : action -> action t

let null act =
 fun () -> try act () with effect Action _, k -> continue k (fun n -> n)

let double act =
 fun () ->
  try act ()
  with effect Action f, k ->
    let g = perform (Action (fun n -> f (f n))) in
    continue k g

let just act =
 fun () -> try act () with effect Action f, k -> continue k (fun n -> f n)

(* Performs g then performs f *)
let prog1 () =
  let f = perform (Action (fun n -> n + 1)) in
  let g = perform (Action (fun n -> n * 2)) in
  fun x -> f (g x)

(* Performs g and f *)
let prog2 () =
  let f = fun n -> n + 1 in
  let g = fun n -> n * 2 in
  perform (Action (fun x -> f (g x)))

(* Doubles all effects inside the program *)
let double_prog prog = prog |> double |> just |> null

let () =
  print_int ((double_prog prog1) () 0);
  print_newline ();
  print_int ((double_prog prog2) () 0)
