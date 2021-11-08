data Nat = O | S Nat
         deriving Eq

data Step = Left Nat
          | Right Nat
          | Fill
          | Pour Nat

data State = St Nat Nat [Nat]

plus a b =
  case a of
    O -> b
    S x -> x `plus` (S b)


ge a b =
  case a of
    O -> b == O
    S x ->
      case b of
        O   -> True
        S y -> x `ge` y


minus a b =
  case b of
    O   -> a
    S y ->
      case a of
        O   -> O
        S x -> x `minus` y


elem l n =
  case l of
    (x:xs) ->
      case n of
        O -> x
        S m -> elem xs m


changeElem l n f =
  case l of
    (x:xs) ->
      case n of
        O -> (f x : xs)
        S m -> (x : changeElem xs m f)


checkStep step state len cop =
  case state of
    St pos fuel sts ->
      case step of
        Left  d -> pos `ge` d && fuel `ge` d && d /= O
        Right d -> len `ge` (pos `plus` d) && fuel `ge` d && d /= O
        Pour  f -> pos /= len && pos /= O && f /= O && fuel `ge` f
        Fill ->
          case pos of
            O -> fuel /= cop
            S x -> fuel /= cop && elem sts x /= O


step stp state len cop =
  case state of
    St pos fuel sts ->
      case stp of
        Left  d -> St (pos `minus` d) (fuel `minus` d) sts
        Right d -> St (pos `plus` d) (fuel `minus` d) sts
        Pour  f ->
          case pos of
            S x -> St pos (fuel `minus` f) (changeElem sts x (\e -> f `plus` e))
        Fill    ->
          case pos of
            O   -> St pos cop sts
            S x ->
              let stationFuel = elem sts x in
              let totalFuel   = fuel `plus` stationFuel in
              if totalFuel `ge` cop
              then St pos cop (changeElem sts x (\e -> totalFuel `minus` cop))
              else St pos totalFuel (changeElem sts x (\e -> O))


isFinishState state len =
  case state of
    St pos fuel sts -> pos == len


getFuel step state cop =
  case step of
    Left d  -> O
    Right d -> O
    Pour f  -> O
    Fill    ->
      case state of
        St pos fuel sts ->
          case pos of
            O -> cop `minus` fuel
            S x -> O


isMove step =
  case step of
    Left  x -> True
    Right x -> True
    Fill    -> False
    Pour  x -> False


checkAnswer answer len cop =
    calcFuel startState answer False
  where
    calcFuel state ans prevIsMove =
      case ans of
        [] -> if isFinishState state len then Just cop else Nothing
        (x:xs) ->
          let currIsMove = isMove x in
          if prevIsMove == currIsMove
          then Nothing
          else
            if checkStep x state len cop
            then
              case calcFuel (step x state len cop) xs currIsMove of
                Nothing -> Nothing
                Just res -> Just (getFuel x state cop `plus` res)
          else Nothing

    startState =
        St O cop (stations len)
      where
        stations n =
          case n of
            O -> []
            S m -> (O : stations m)

