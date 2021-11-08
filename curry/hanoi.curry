data Nat = Z | S Nat  
         deriving Eq

data Stick = One | Two | Thr 
           deriving Eq

data Triple a = Triple (a, a, a)

less :: Nat -> Nat -> Bool 
less a b = 
  case b of 
    Z -> False 
    S b' -> case a of 
              Z -> True 
              S a' -> less a' b' 

get :: Stick -> Triple a -> a 
get name state = 
  case state of 
    Triple (s1, s2, s3) -> 
      case name of 
        One -> s1 
        Two -> s2
        Thr -> s3 

set :: Stick -> a -> Triple a -> Triple a 
set name stack state = 
  case state of 
    Triple (s1, s2, s3) -> 
      case name of 
        One -> Triple (stack, s2, s3)
        Two -> Triple (s1, stack, s3) 
        Thr -> Triple (s1, s2, stack)

oneStep :: (Stick, Stick) -> Triple [Nat] -> Triple [Nat]
oneStep step state = 
  case step of 
    (fromN, toN) -> 
      case fromN /= toN of 
        True -> 
          case get fromN state of 
            (x:xs) -> 
              case get toN state of 
                [] -> set toN [x] (set fromN xs state)
                r@(y:ys) -> 
                  case less x y of 
                    True -> set toN (x : r) (set fromN xs state)
                 
check :: Triple [Nat] -> [(Stick, Stick)] -> Bool
check state steps = 
  case steps of 
    [] -> get One state == [] && get Two state == [] 
    (x:xs) -> check (oneStep x state) xs

zero = Z 
one = S zero 
two = S one 
three = S two 
four = S three 
five = S four

startState = Triple ([zero, one, two, three], [], []) 
-- startState = Triple ([zero, one, two, three, four, five], [], []) 

-- works for 3 disks
answer = [(One, Thr), (One, Two), (Thr, Two), (One, Thr), (Two, One), (Two, Thr), (One, Thr)] 

answer1 = [(One, Two), (One, Thr), (Two, Thr), (One, Two), (Thr, One), (Thr, Two), (One, Two), (One, Thr), (Two, Thr), (Two, One), (Thr, One), (Two, Thr), (One, Two), (One, Thr), (Two, Thr)]

-- (check startState answer where answer free) =:= True









