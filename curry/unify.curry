
data Nat  = O | S Nat deriving Eq
data Term = Var_ Nat | Constr Nat [Term] deriving Eq

get_term var subst =
  case subst of
    (x:xs) ->
      case var of
          O   -> x
          S n -> get_term n xs


forall2 f l1 l2 =
  case (l1, l2) of
    ([], []) -> True
    ((x:xs), (y:ys)) -> f x y && forall2 f xs ys


check_uni_f subst ft t =
  case (ft, t) of
    (Constr n1 a1, Constr n2 a2) -> n1 == n2 && forall2 (check_uni_f subst) a1 a2
    (_, Var_ v) -> ft == get_term v subst


check_uni subst t1 t2 =
  case (t1, t2) of
    (Constr n1 a1, Constr n2 a2) -> n1 == n2 && forall2 (check_uni subst) a1 a2
    (Var_ v, Constr n a) -> check_uni_f subst (get_term v subst) t2
    (Constr n a, Var_ v) -> check_uni_f subst (get_term v subst) t1
    (Var_ v1, Var_ v2) -> get_term v1 subst == get_term v2 subst