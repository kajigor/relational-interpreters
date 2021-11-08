data Bottle = Fst | Snd
data StepType = Fill | Empty | Pour
data Nat = O | S Nat deriving Eq


add a b =
  case a of
    O -> b
    S x -> add x (S b)


greater a b =
  case a of
    O -> False
    S x ->
      case b of
        O -> True
        S y -> greater x y


sub a b =
  case b of
    O -> a
    S y ->
      case a of
        O -> O
        S x -> sub x y


anotherBottle b =
  case b of
    Fst -> Snd
    Snd -> Fst


createState bottle lvl1 lvl2 =
   case bottle of
     Fst -> (lvl1, lvl2)
     Snd -> (lvl2, lvl1)


checkStep (f, s) (t, b) capacities =
  let lvl1 = case b of Fst -> f ; Snd -> s in
  let lvl2 = case b of Fst -> s ; Snd -> f in
  case t of
    Fill -> lvl1 == O
    Empty -> lvl1 == capacities b
    Pour ->
      let b' = anotherBottle b in
      not (lvl1 == O || lvl2 == capacities b')

doStep (f, s) (t, b) capacities =
  let lvl2 = case b of Fst -> s ; Snd -> f in
  case t of
    Fill -> createState b (capacities b) lvl2
    Empty -> createState b O lvl2
    Pour ->
      let sum  = add f s in
      let cap2 = capacities (anotherBottle b) in
      if greater sum cap2
      then createState b (sub sum cap2) cap2
      else createState b O sum

isFinishState (f, s) reqLvl = f == reqLvl || s == reqLvl

checkAnswer answer capacities reqLvl =
    let startState = (O, O) in
    checkAnswer startState answer
  where
    checkAnswer state0 x =
      case x of
        [] -> isFinishState state0 reqLvl
        (x : xs) ->
          if checkStep state0 x capacities
          then checkAnswer (doStep state0 x capacities) xs
          else False

------------------------------------------------------------------------

capacities1 b =
  case b of
    Fst -> S (S (S (S O)))
    Snd -> S (S (S (S (S (S (S (S (S O))))))))