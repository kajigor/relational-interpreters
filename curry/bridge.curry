data Peano = O | S Peano 
data Person = A | B | C | D 
data Step = One Person | Two Person Person 

greater :: Peano -> Peano -> Bool
greater a0 b0 = 
  case a0 of 
    O -> False 
    S x -> 
      case b0 of 
        O -> True 
        S y -> greater x y 

grForPerson :: Person -> Person -> Bool
grForPerson x y = 
  case x of 
    A -> case y of 
           A -> False 
           B -> True 
           C -> True 
           D -> True 
    B -> case y of    
           A -> False
           B -> False 
           C -> True 
           D -> True 
    C -> case y of 
           A -> False
           B -> False 
           C -> False 
           D -> True 
    D -> False 

maximum :: Peano -> Peano -> Peano 
maximum a0 b0 = 
  if greater a0 b0 then a0 else b0 

add :: Peano -> Peano -> Peano 
add a0 b0 = 
  case a0 of 
    O -> b0 
    S x -> add x (S b0)

checkPerson :: Eq a => (a, a, a, a, a) -> Person -> Bool
checkPerson (l, a0, b0, c0, d0) x = 
  case x of 
    A -> a0 == l 
    B -> b0 == l 
    C -> c0 == l 
    D -> d0 == l 

checkStep :: Eq a => (a, a, a, a, a) -> Step -> Bool
checkStep state st = 
  case st of 
    One p -> checkPerson state p 
    Two p q -> checkPerson state p && checkPerson state q && grForPerson p q 

moveLight :: (Bool, a, b, c, d) -> (Bool, a, b, c, d)
moveLight (l, a0, b0, c0, d0) = (not l, a0, b0, c0, d0)

movePerson :: (a, Bool, Bool, Bool, Bool) 
           -> Person 
           -> (a, Bool, Bool, Bool, Bool)
movePerson (l, a0, b0, c0, d0) p = 
  case p of 
    A -> (l, not a0, b0, c0, d0)
    B -> (l, a0, not b0, c0, d0)
    C -> (l, a0, b0, not c0, d0)
    D -> (l, a0, b0, c0, not d0)

step :: (Bool, Bool, Bool, Bool, Bool) 
     -> Step 
     -> (Bool, Bool, Bool, Bool, Bool)
step state st = 
  case st of 
    One p -> moveLight (movePerson state p)
    Two p q -> moveLight (movePerson (movePerson state p) q)

getTime :: Step -> (Person -> Peano) -> Peano
getTime state times = 
  case state of 
    One p -> times p 
    Two p q -> maximum (times p) (times q) 

getAnswer :: [Step] -> (Person -> Peano) -> Maybe Peano
getAnswer answer times = 
    go answer start 
  where
    finish = (False, False, False, False, False) 
    start = (True, True, True, True, True) 
    go ans state =  
      case ans of 
        (x : xs) -> 
           if checkStep state x 
           then 
             case (go xs (step state x)) of 
               Nothing -> Nothing 
               Just t1 -> Just (add (getTime x times) t1)
           else 
             Nothing 
        [] -> if state == finish then Just O else Nothing 

one :: Peano
one = S O 

two :: Peano 
two = S (S O)

five :: Peano 
five = S (S (S (S (S O))))

ten :: Peano 
ten = S (S (S (S (S (S (S (S (S (S O)))))))))

seventeen :: Peano 
seventeen = toPeano 17

toPeano :: Int -> Peano 
toPeano 0 = O 
toPeano n | n > 0 = S (toPeano (n-1))
toPeano n | n < 0 = error "failed to convert negative number to Peano"
 
standardTimes :: Person -> Peano 
standardTimes p = 
  case p of 
    A -> one 
    B -> two 
    C -> five 
    D -> ten 

-- (getAnswer answ standardTimes) =:= Just seventeen where answ free
-- does not terminate:
