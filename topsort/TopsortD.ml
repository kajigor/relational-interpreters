module L = List

open OCanren
open OCanren.Std
open GT
open Topsort

@type 'a gnat    = 'a Topsort.gnat = O | S of 'a with show, gmap
@type lnat       = lnat gnat logic               with show, gmap
@type lnumbering = lnat Std.List.logic           with show, gmap
@type graph      = (int * int) list              with show

let rec to_nat n = if n = 0 then o () else s @@ to_nat @@ n - 1

let rec to_graph = function
  []           -> nil ()
| (b, e) :: tl -> pair (to_nat b) (to_nat e) % to_graph tl

let rec reify_nat h n = For_gnat.reify reify_nat h n;;

Random.self_init ();;

let split n =
  let m = 1 + Random.int n in
  let rec init acc rest i =
  match i with
  | 1 -> rest :: acc
  | k -> let x = if rest = 1 then 1 else 1 + Random.int rest in
         init (x :: acc) (rest-x+1) (k-1)
  in
  init [] (n-m+1) m

let tree n =
  let rec inner (acc, root) = function
  | 1 -> (acc, root+1)
  | n ->
     let subs = split n in
     L.fold_left
       (fun (acc, i) n ->
          inner ((root, i) :: acc, i) n
       )
       (acc, root+1)
       subs
  in
  fst @@ inner ([], 0) (n-1);;

let dag tree m n =
  tree @
  L.init m
    (fun _ ->
      let rec gen _ =
        let x, y = Random.int n, Random.int n in
        if Stdlib.(<) x y then (x, y) else gen ()
      in
      gen ()
    )

let to_dot n g =
  let b   = Buffer.create 512 in
  let add = Buffer.add_string b in
  add "digraph X {\n";
  L.iter (fun (x, y) -> add @@ Printf.sprintf "  %s -> %s\n" (show(lnat) @@ L.nth n x) (show(lnat) @@ L.nth n y)) g;
  add "}\n";
  Buffer.contents b

let topsort g =
  let before = Sys.time() in
  let order =
    L.hd @@ Stream.take ~n:1 @@
    run q
        (fun q -> eval (to_graph g) q)
        (fun a -> a#reify (Std.List.reify reify_nat)) in
  let after = Sys.time() in
  Printf.printf "\n";
  Printf.printf "Graph  : %s\n" @@ show(graph) g;
  Printf.printf "Topsort: %s\n" @@
  show(lnumbering) order;
  Printf.printf "Time   : %fs\n" @@ (after -. before)

let topsort_3 g =
  let before = Sys.time() in
  let order =
    L.hd @@ Stream.take ~n:1 @@
    run q
        (fun q -> eval_3 (to_graph g) q)
        (fun a -> a#reify (Std.List.reify reify_nat)) in
  let after = Sys.time() in
  Printf.printf "\nCompletely unfolded eval for graph with 3 edges\n";
  Printf.printf "Graph  : %s\n" @@ show(graph) g;
  Printf.printf "Topsort: %s\n" @@
  show(lnumbering) order;
  Printf.printf "Time   : %fs\n" @@ (after -. before)


let topsort_15 g =
  let before = Sys.time() in
  let order =
    L.hd @@ Stream.take ~n:1 @@
    run q
        (fun q -> eval_15 (to_graph g) q)
        (fun a -> a#reify (Std.List.reify reify_nat)) in
  let after = Sys.time() in
  Printf.printf "\nCompletely unfolded eval for graph with 15 edges\n";
  Printf.printf "Graph  : %s\n" @@ show(graph) g;
  Printf.printf "Topsort: %s\n" @@
  show(lnumbering) order;
  Printf.printf "Time   : %fs\n" @@ (after -. before)

let gen n =
  let bind xs f = L.concat_map f xs in
  let return x = [x] in
  let rec range l u =
    if l >= u
    then []
    else l :: (range (l + 1) u) in
  let compare (x, y) (x', y') =
    let r = Int.compare x x' in
    if r <= 0
    then Int.compare y y'
    else r in
  L.sort compare @@ bind (range 1 n) (fun y -> bind (range 0 y) (fun x -> return (x,y)))

let flip xs =
  L.map (fun (x, y) -> (y, x)) xs

let _ =
  L.iter topsort
    [
     [0, 1; 0, 2];
     [0, 1; 0, 2; 1, 2];
     [0, 1; 0, 2; 2, 1];
     [0, 1; 0, 2; 2, 3; 1, 3; 1, 2];

     (gen 6);
     L.rev (gen 6);

     (gen 7);
     L.rev (gen 7);

     (flip @@ gen 6);
     L.rev (flip @@ gen 6);
    ];
  L.iter topsort_15
    [
     (flip @@ gen 6);
     L.rev (flip @@ gen 6);
    ];
  L.iter topsort_3
    [
     [0, 1; 0, 2; 1, 2];
     [0, 1; 0, 2; 2, 1];
    ]
(*
     (*
        (* high-order *)
        Graph  : [(4, 5); (3, 5); (3, 4); (2, 5); (2, 4); (2, 3); (1, 5); (1, 4); (1, 3); (1, 2); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); _.376911]

        real	0m0.745s
        user	0m0.741s
        sys	0m0.004s

        (* no high-order *)
        Graph  : [(4, 5); (3, 5); (3, 4); (2, 5); (2, 4); (2, 3); (1, 5); (1, 4); (1, 3); (1, 2); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); _.22727]

        real	0m0.117s
        user	0m0.109s
        sys	0m0.008s
     *)
     L.rev [0, 1; 0, 2; 0, 3; 0, 4; 0, 5;
      1, 2; 1, 3; 1, 4; 1, 5;
      2, 3; 2, 4; 2, 5;
      3, 4; 3, 5;
      4, 5;
     ]; *)

(*
     (*
        (* high-order *)
        Graph  : [(0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (1, 2); (1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5); (3, 4); (3, 5); (4, 5)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); _.2205437]

        real	0m14.109s
        user	0m14.084s
        sys	0m0.024s

        (* no high-order *)
        Graph  : [(0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (1, 2); (1, 3); (1, 4); (1, 5); (2, 3); (2, 4); (2, 5); (3, 4); (3, 5); (4, 5)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); _.165465]

        real	0m0.882s
        user	0m0.873s
        sys	0m0.008s

     *)
     [0, 1; 0, 2; 0, 3; 0, 4; 0, 5;
      1, 2; 1, 3; 1, 4; 1, 5;
      2, 3; 2, 4; 2, 5;
      3, 4; 3, 5;
      4, 5;
     ]; *)

(*
     (*
        (* high-order *)
        Graph  : [(5, 6); (4, 6); (4, 5); (3, 6); (3, 5); (3, 4); (2, 6); (2, 5); (2, 4); (2, 3); (1, 6); (1, 5); (1, 4); (1, 3); (1, 2); (0, 6); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); S (S (S (S (S (S (O)))))); _.1638365]

        real	0m5.171s
        user	0m5.159s
        sys	0m0.012s

        (* no high-order *)
        Graph  : [(5, 6); (4, 6); (4, 5); (3, 6); (3, 5); (3, 4); (2, 6); (2, 5); (2, 4); (2, 3); (1, 6); (1, 5); (1, 4); (1, 3); (1, 2); (0, 6); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); S (S (S (S (S (S (O)))))); _.75974]

        real	0m0.599s
        user	0m0.599s
        sys	0m0.000s

     *)
     L.rev [0, 1; 0, 2; 0, 3; 0, 4; 0, 5; 0, 6;
      1, 2; 1, 3; 1, 4; 1, 5; 1, 6;
      2, 3; 2, 4; 2, 5; 2, 6;
      3, 4; 3, 5; 3, 6;
      4, 5; 4, 6;
      5, 6;
     ]; *)
(*
     (*
        (* high-order *)
        Graph  : [(0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (2, 3); (2, 4); (2, 5); (2, 6); (3, 4); (3, 5); (3, 6); (4, 5); (4, 6); (5, 6)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); S (S (S (S (S (S (O)))))); _.27978070]

        real	6m54.900s
        user	6m54.055s
        sys	0m0.800s

        (* no high-order *)
        Graph  : [(0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (2, 3); (2, 4); (2, 5); (2, 6); (3, 4); (3, 5); (3, 6); (4, 5); (4, 6); (5, 6)]
        Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); S (S (S (S (S (S (O)))))); _.1499948]

        real	0m24.368s
        user	0m24.347s
        sys	0m0.024s

     *)
     [0, 1; 0, 2; 0, 3; 0, 4; 0, 5; 0, 6;
      1, 2; 1, 3; 1, 4; 1, 5; 1, 6;
      2, 3; 2, 4; 2, 5; 2, 6;
      3, 4; 3, 5; 3, 6;
      4, 5; 4, 6;
      5, 6;
     ]; *)

(*
Graph  : [(6, 7); (5, 7); (5, 6); (4, 7); (4, 6); (4, 5); (3, 7); (3, 6); (3, 5); (3, 4); (2, 7); (2, 6); (2, 5); (2, 4); (2, 3); (1, 7); (1, 6); (1, 5); (1, 4); (1, 3); (1, 2); (0, 7); (0, 6); (0, 5); (0, 4); (0, 3); (0, 2); (0, 1)]
Topsort: [O; S (O); S (S (O)); S (S (S (O))); S (S (S (S (O)))); S (S (S (S (S (O))))); S (S (S (S (S (S (O)))))); S (S (S (S (S (S (S (O))))))); _.232702]

real	0m4.241s
user	0m4.236s
sys	0m0.004s

 *)

     (* L.rev [0, 1; 0, 2; 0, 3; 0, 4; 0, 5; 0, 6;
      1, 2; 1, 3; 1, 4; 1, 5; 1, 6;
      2, 3; 2, 4; 2, 5; 2, 6;
      3, 4; 3, 5; 3, 6;
      4, 5; 4, 6;
      5, 6;
     ]; *)
(*   (* more than 10 minutes*)
     [0, 1; 0, 2; 0, 3; 0, 4; 0, 5; 0, 6; 0, 7; 0, 8;
      1, 2; 1, 3; 1, 4; 1, 5; 1, 6; 1, 7; 1, 8;
      2, 3; 2, 4; 2, 5; 2, 6; 2, 7; 2, 8;
      3, 4; 3, 5; 3, 6; 3, 7; 3, 8;
      4, 5; 4, 6; 4, 7; 4, 8;
      5, 6; 5, 7; 5, 8;
      6, 7; 6, 8;
      7, 8;
     ] *)





