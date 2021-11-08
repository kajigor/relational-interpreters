data Expression a = I
                  | O
                  | Var a
                  | Not (Expression a)
                  | Conj (Expression a) (Expression a)
                  | Disj (Expression a) (Expression a)

(>>=) e f =
  case e of
    Nothing -> Nothing
    Just x -> f x


lookup s n =
  case s of
    [] -> Nothing
    ((m, e):xs) -> if n == m then Just e else lookup xs n


eval subst expr =
  case expr of
    I -> Just True
    O -> Just False
    Var x -> lookup subst x
    Not e -> eval subst e >>= \b -> Just (not b)
    Conj l r -> eval subst l >>= \a -> eval subst r >>= \b -> Just (a && b)
    Disj l r -> eval subst l >>= \a -> eval subst r >>= \b -> Just (a || b)


conj_True a b =
  case a of
    True -> b


append a b =
  case a of
    [] -> b
    (x:xs) -> (x : append xs b)

remove v l =
  case l of
    [] -> []
    (x:xs) -> let nl = remove v xs in if v == x then nl else (x : nl)

remove_repeats l =
  case l of
    [] -> []
    x:xs -> x : remove_repeats (remove x xs)

all_vars e =
  case e of
    I -> []
    O -> []
    Var x -> [x]
    Not e -> all_vars e
    Conj l r -> append (all_vars l) (all_vars r)
    Disj l r -> append (all_vars l) (all_vars r)

check_subst subst expr =
    check subst (all_vars expr)
  where
    check subst vars =
      case vars of
        [] -> subst == []
        (x:xs) ->
          case subst of
            ((a, b):ys) -> conj_True (x == a) (check ys xs)

check_and_eval subst expr =
  Just (check_subst subst expr) >>= \x -> eval subst expr >>= \y -> Just (x && y)

-----------------------------------------------------------------------------------------

expr1 = O
expr2 = Disj (Var "ok") I
expr3 = Conj (Var "ok") I

expr4 =
  Disj (Conj (Var "X") (Var "Y")) (Conj (Var "Z") (Not (Var "Last")))