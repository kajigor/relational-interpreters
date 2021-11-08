data Prop = Lit Bool | Conj Prop Prop | Disj Prop Prop 

eval :: Prop -> Bool 
eval (Lit x) = x 
eval (Conj l r) = eval l && eval r 
eval (Disj l r) = eval l || eval r 

depth :: Prop -> Int
depth (Conj l r) = 1 + max (depth l) (depth r) 
depth (Disj l r) = 1 + max (depth l) (depth r) 
depth (Lit _) = 1 

-- (depth x <= 4 && eval x) =:= True where x free

withSub e = e 
          ? op (withSub e) unknown 
          ? op unknown (withSub e)
  where op = Conj ? Disj

lits (withSub (Lit x)) = x 
