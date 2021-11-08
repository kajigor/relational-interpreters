data Nat = Z | S Nat
data Set = Set [Nat] [Nat] [Nat]
data Pin = A | B | C deriving Eq


less x (S y') =
  case x of
    Z    -> True
    S x' -> less x' y'

extra (A, B) = C
extra (B, A) = C
extra (A, C) = B
extra (C, A) = B
extra (B, C) = A
extra (C, B) = A

select (Set pin1 pin2 pin3) pin =
  case pin of
    A -> pin1
    B -> pin2
    C -> pin3

permut move s =
  case move of
    (x, y) -> Set (select s x) (select s y) (select s (extra move))

tumrep move s =
  case s of
    Set x y z ->
      case move of
        (A, B) -> Set x y z
        (B, A) -> Set y x z
        (A, C) -> Set x z y
        (C, A) -> Set y z x
        (B, C) -> Set z x y
        (C, B) -> Set z y x

check s p = eval p s

eval p s =
  case p of
    [] -> s
    (move : p') ->
      case move of
        (x, y) ->
          eval p' (
            if x == y
            then s
            else
              case permut move s of
                Set (topA : restA) onB onC ->
                  tumrep move (
                    case onB of
                      [] -> Set restA [topA] onC
                      (topB : _) ->
                        case less topA topB of
                          True -> Set restA (topA : onB) onC))

zero = Z
one = S zero
two = S one
three = S two
four = S three
five = S four

pin = [zero, one, two]

startState = Set pin [] []
finishState = Set [] [] pin

-- check startState answer =:= finishState where answer free
-- succeeds at 2 rings, not fails at 3 rings (dfs and bfs)