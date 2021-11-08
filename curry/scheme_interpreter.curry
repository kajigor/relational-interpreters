data Variable = First | Next Variable deriving Eq

data Identifier = Lambda | Quote | List | Var Variable deriving Eq

data Term = Ident Identifier | Seq [Term]

data Result = Val Term | Closure Identifier Term [(Identifier, Result)]

map f l =
  case l of
    (x : xs) -> (f x : map f xs)
    [] -> []

lookup x env =
  case env of
    ((y, res) : env') -> if x == y then res else lookup x env'

not_in_env x env =
  case env of
    [] -> True
    ((y, res) : env') ->
      if x == y then False else not_in_env x env'

eval term env =
    case term of
      Ident x -> lookup x env
      Seq (t : ts) ->
        case t of
          Ident id ->
            case id of
              Lambda -> lambda_handler ts env
              Quote -> quote_handler ts env
              List -> list_handler ts env
          Seq s ->
            case ts of
              [arg] ->
                case eval t env of
                  Closure x body env' -> eval body ((x, eval arg env) : env')
  where
    lambda_handler ts env =
      case not_in_env Lambda env of
        True ->
          case ts of
            [Seq [Ident i], body] -> Closure i body env

    quote_handler ts env =
      case not_in_env Quote env of
        True ->
          case ts of
            [t] -> Val t

    list_handler ts env =
      case not_in_env List env of
        True ->
          let eval_val t = case eval t env of Val v -> v in
          Val (Seq (map eval_val ts))

