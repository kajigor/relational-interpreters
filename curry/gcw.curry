data Person = G | C | W | N

checkState :: (Bool, Bool, Bool, Bool) -> Bool
checkState (i0, g0, c0, w0) =
  if i0 == g0
  then True
  else
    if i0 == c0
    then i0 == w0
    else False

checkStep :: (Bool, Bool, Bool, Bool) -> Person -> Bool
checkStep (i0, g0, c0, w0) step =
  case step of
    N -> True
    G -> i0 == g0
    C -> i0 == c0
    W -> i0 == w0

makeStep :: (Bool, Bool, Bool, Bool)
         -> Person
         -> (Bool, Bool, Bool, Bool)
makeStep (i0, g0, c0, w0) step =
  case step of
    G -> (not i0, not g0, c0, w0)
    C -> (not i0, g0, not c0, w0)
    W -> (not i0, g0, c0, not w0)
    N -> (not i0, g0, c0, w0)

checkAnswer :: [Person] -> Bool
checkAnswer ans =
    go ans startState
  where
    startState  = (True, True, True, True)
    finishState = (False, False, False, False)
    go a state =
      case a of
        [] -> state == finishState
        (x:xs) ->
          if checkStep state x
          then
            let newState = makeStep state x in
            if checkState newState
            then go xs newState
            else False
          else False

-- Not sure why it does not generate the answer, but can check it is correct
-- gcw> checkAnswer [G, N, W, G, C, N, G]
-- True

-- gcw> checkAnswer ans =:= True where ans free
-- No more values.
