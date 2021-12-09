open GT
open OCanren
open OCanren.Std
type 'a0 gnat =
  | O
  | S of 'a0
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))
let rec less x y r =
    conde [
      (fresh (y')
        (y === (s y'))
        (conde [
          (x === (o ())) &&& (r === (!! true)) ;
          fresh (x') (x === (s x')) (less x' y' r)
        ]));
      ((y === (o ())) &&& (r === (!! false)))
    ]
let rec less_true x y =
    (fresh (y')
      (y === (s y'))
      (conde [
        (x === (o ())) ;
        fresh (x') (x === (s x')) (less_true x' y')
      ]))

let max x y r =
  fresh (t)
    (less x y t)
    (conde [
      (t === (!! true)) &&& (r === y);
      (t === (!! false)) &&& (r === x)
    ])
let numberOfnodes g r =
  let rec inner acc g r =
      conde [
        ((g === (nil ())) &&& (acc === r)) ;
        (fresh (x y tl xy accxy) (g === ((pair x y) % tl)) (max x y xy) (max acc xy accxy) (inner accxy tl r))
       ] in
  inner (o ()) g r
let rec lookup xs key value =
  fresh (h tl) (xs === (h % tl))
    (((key === (o ())) &&& (h === value)) ||| (fresh (k) (key === (s k)) (lookup tl k value)))

(* must evaluate to true!*)
let eval graph numbering =
     let rec eval graph numbering n =
         conde [
           graph === (nil ());
           (fresh (b e graph') (graph === ((pair b e) % graph'))
              (fresh (nb ne)
                (lookup numbering b nb)
                (lookup numbering e ne)
                (less_true nb ne)
                (less_true ne n)
                (eval graph' numbering n )))
           ] in
     (fresh (n n') (n' === (s n)) (numberOfnodes graph n) (eval graph numbering n'))


(* open GT
open OCanren
open OCanren.Std
type 'a0 gnat =
  | O
  | S of 'a0
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))
let rec less x y q8 =
  fresh (q1) (y q1)
    ((fresh (y' q3) (q1 === (s y')) (x q3) (((q3 === (o ())) &&& (q8 === (!! true))) ||| (fresh (x') (q3 === (s x')) (less (fun q5 -> x' === q5) (fun q6 -> y' === q6) q8)))) |||
       ((q1 === (o ())) &&& (q8 === (!! false))))
let max x y q11 =
  fresh (q12 q13 q9) (x q12) (y q13) (less (fun q14 -> q14 === q12) (fun q15 -> q15 === q13) q9)
    (conde [(q9 === (!! true)) &&& (q11 === q13); (q9 === (!! false)) &&& (q11 === q12)])
let numberOfnodes g q25 =
  let rec inner acc q16 q23 =
    fresh (q19) (q16 q19)
      (((q19 === (nil ())) &&& (acc q23)) |||
         (fresh (x y tl) (q19 === ((pair x y) % tl)) (inner (max acc (max (fun q20 -> x === q20) (fun q21 -> y === q21))) (fun q22 -> tl === q22) q23))) in
  inner (fun q24 -> q24 === (o ())) g q25
let rec lookup q26 q27 q28 =
  fresh (q29 h tl q32) (q29 === (h % tl)) (q26 q29) (q27 q32)
    (((q32 === (o ())) &&& (h === q28)) ||| (fresh (k) (q32 === (s k)) (lookup (fun q35 -> tl === q35) (fun q33 -> k === q33) q28)))
let eval graph numbering q55 =
  fresh (q56) (graph q56)
    (let n q37 = fresh (q36) (q37 === (s q36)) (numberOfnodes (fun q57 -> q57 === q56) q36) in
     let rec eval graph numbering q52 =
       fresh (q53 q39) (numbering q53) (graph q39)
         (((q39 === (nil ())) &&& (q52 === (!! true))) |||
            (fresh (b e graph') (q39 === ((pair b e) % graph'))
               (let nb = lookup (fun q54 -> q54 === q53) (fun q49 -> b === q49) in
                let ne = lookup (fun q54 -> q54 === q53) (fun q50 -> e === q50) in
                fresh (q47) (less nb ne q47)
                  (conde
                     [(q47 === (!! false)) &&& (q52 === (!! false));
                     fresh (q43) (q47 === (!! true)) (less ne n q43)
                       (conde [(q43 === (!! false)) &&& (q52 === (!! false)); (q43 === (!! true)) &&& (eval (fun q51 -> graph' === q51) (fun q54 -> q54 === q53) q52)])])))) in
     eval (fun q57 -> q57 === q56) numbering q55) *)