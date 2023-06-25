open BaseTypes

let ( << ) f g x = f (g x)

let rec ( <*> ) (a1 : action) (a2 : action) : action =
  match (a1, a2) with
  | Instant f1, Instant f2 -> Instant (f1 << f2)
  | Targetted f1, _ -> Targetted (fun action -> f1 action <*> a2)
  | _, Targetted f2 -> Targetted (fun action -> a1 <*> f2 action)

let rec apply_action action state targets =
  match (action : action) with
  | Instant f -> f state
  | Targetted f -> apply_action (f (List.hd targets)) state (List.tl targets)
