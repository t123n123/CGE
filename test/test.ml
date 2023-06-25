open CGE
open QCheck

(* O_O *)
let ( $ ) = ( @@ )

(* Example properties and unit tests *)
let prop_replace =
  QCheck.Test.make ~count:1000 ~name:"Replaces are equivalent"
    (tup2 (list int) small_nat)
    (fun (l, i) ->
      assume (List.length l > i);
      Utils.replace l i 0 = Utils.replace l i 0)

let prop_remove =
  QCheck.Test.make ~count:1000 ~name:"Removes are equivalent"
    (tup2 (list int) small_nat)
    (fun (l, i) ->
      assume (List.length l > i && l <> []);
      Utils.remove l i = Utils.remove2 l i)

let test_replace () =
  let l = [ 1; 2; 3; 4; 5 ] in
  let result = Utils.replace l 2 0 in
  Alcotest.(check $ list int) "Test result" [ 1; 2; 0; 4; 5 ] result

let test_replace2 () =
  let l = [ 1; 2; 3; 4; 5 ] in
  let result = Utils.replace l 2 0 in
  Alcotest.(check $ list int) "Test result" [ 1; 2; 0; 4; 5 ] result

let () =
  let open Alcotest in
  let qcheck_props = [ prop_replace; prop_remove ] in
  run "Engine Tests"
    [
      ( "Replace Tests",
        [
          test_case "Replace Test" `Quick test_replace;
          test_case "Replace2 Test" `Quick test_replace2;
        ] );
      ("QuickCheck Tests", List.map QCheck_alcotest.to_alcotest qcheck_props);
    ]
