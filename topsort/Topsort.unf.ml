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
  fresh (h tl)
    (xs === (h % tl))
    (conde [
      (key === (o ())) &&& (h === value);
      fresh (k) (key === (s k)) (lookup tl k value)
    ])


let rec lookup_lookup xs key1 value1 key2 value2 =
  fresh (h tl k1 k2)
    (xs === (h % tl))
    (conde [
      (key1 === (o ())) &&& (h === value1) &&&
        (conde [
          (key2 === (o ())) &&& (h === value2);
          (key2 === (s k2)) &&& (lookup tl k2 value2)
        ]);
      (key1 === (s k1)) &&&
        (conde [
          (key2 === (o ())) &&& (h === value2) &&& (lookup tl k1 value1);
          (key2 === (s k2)) &&& (lookup_lookup tl k1 value1 k2 value2)
        ])
    ])

(* must evaluate to true!*)
let eval graph numbering =
     let rec eval graph numbering n =
         conde [
           graph === (nil ());
           (fresh (b e graph') (graph === ((pair b e) % graph'))
              (fresh (nb ne)
                (lookup_lookup numbering b nb e ne)
                (less_true nb ne)
                (less_true ne n)
                (eval graph' numbering n )))
           ] in
     (fresh (n n') (n' === (s n)) (numberOfnodes graph n) (eval graph numbering n'))


let eval_15 graph numbering =
     let rec eval graph numbering n =
         conde [
           graph === (nil ());
           (fresh (b1 e1 b2 e2 b3 e3 b4 e4 b5 e5 b6 e6 b7 e7 b8 e8 b9 e9 b10 e10 b11 e11 b12 e12 b13 e13 b14 e14 b15 e15)
             (graph === ((pair b1 e1) % ((pair b2 e2) % ((pair b3 e3) % ((pair b4 e4) % ((pair b5 e5) % ((pair b6 e6) % ((pair b7 e7) % ((pair b8 e8) % ((pair b9 e9) % ((pair b10 e10) % ((pair b11 e11) % ((pair b12 e12) % ((pair b13 e13) % ((pair b14 e14) % ((pair b15 e15) % (nil ())))))))))))))))))
              (fresh (nb1 ne1 nb2 ne2 nb3 ne3 nb4 ne4 nb5 ne5 nb6 ne6 nb7 ne7 nb8 ne8 nb9 ne9 nb10 ne10 nb11 ne11 nb12 ne12 nb13 ne13 nb14 ne14 nb15 ne15)
                (lookup numbering b1 nb1)
                (lookup numbering b2 nb2)
                (lookup numbering b3 nb3)
                (lookup numbering b4 nb4)
                (lookup numbering b5 nb5)
                (lookup numbering b6 nb6)
                (lookup numbering b7 nb7)
                (lookup numbering b8 nb8)
                (lookup numbering b9 nb9)
                (lookup numbering b10 nb10)
                (lookup numbering b11 nb11)
                (lookup numbering b12 nb12)
                (lookup numbering b13 nb13)
                (lookup numbering b14 nb14)
                (lookup numbering b15 nb15)
                (lookup numbering e1 ne1)
                (lookup numbering e2 ne2)
                (lookup numbering e3 ne3)
                (lookup numbering e4 ne4)
                (lookup numbering e5 ne5)
                (lookup numbering e6 ne6)
                (lookup numbering e7 ne7)
                (lookup numbering e8 ne8)
                (lookup numbering e9 ne9)
                (lookup numbering e10 ne10)
                (lookup numbering e11 ne11)
                (lookup numbering e12 ne12)
                (lookup numbering e13 ne13)
                (lookup numbering e14 ne14)
                (lookup numbering e15 ne15)
                (less_true nb1 ne1)
                (less_true ne1 n)
                (less_true nb2 ne2)
                (less_true ne2 n)
                (less_true nb3 ne3)
                (less_true ne3 n)
                (less_true nb4 ne4)
                (less_true ne4 n)
                (less_true nb5 ne5)
                (less_true ne5 n)
                (less_true nb6 ne6)
                (less_true ne6 n)
                (less_true nb7 ne7)
                (less_true ne7 n)
                (less_true nb8 ne8)
                (less_true ne8 n)
                (less_true nb9 ne9)
                (less_true ne9 n)
                (less_true nb10 ne10)
                (less_true ne10 n)
                (less_true nb11 ne11)
                (less_true ne11 n)
                (less_true nb12 ne12)
                (less_true ne12 n)
                (less_true nb13 ne13)
                (less_true ne13 n)
                (less_true nb14 ne14)
                (less_true ne14 n)
                (less_true nb15 ne15)
                (less_true ne15 n)))
           ] in
     (fresh (n n') (n' === (s n)) (numberOfnodes graph n) (eval graph numbering n'))

let eval_3 graph numbering =
     let rec eval graph numbering n =
         conde [
           graph === (nil ());
           (fresh (b1 e1 b2 e2 b3 e3)
             (graph === ((pair b1 e1) % ((pair b2 e2) % ((pair b3 e3) % (nil ())))))
              (fresh (nb1 ne1 nb2 ne2 nb3 ne3)
                (lookup numbering b1 nb1)
                (lookup numbering b2 nb2)
                (lookup numbering b3 nb3)
                (lookup numbering e1 ne1)
                (lookup numbering e2 ne2)
                (lookup numbering e3 ne3)
                (less_true nb1 ne1)
                (less_true ne1 n)
                (less_true nb2 ne2)
                (less_true ne2 n)
                (less_true nb3 ne3)
                (less_true ne3 n)))
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