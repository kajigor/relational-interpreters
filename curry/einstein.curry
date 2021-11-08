data HouseColor  = Yellow    | Blue         | Red        | Ivory       | Green      deriving Eq
data Nationality = Norwegian | Ukrainian    | Englishman | Spaniard    | Japanese   deriving Eq
data Drink       = Water     | Tea          | Milk       | OrangeJuice | Coffee     deriving Eq
data Smoke       = Kools     | Chesterfield | OldGold    | LackyStrike | Parliament deriving Eq
data Pet         = Fox       | Hourse       | Snails     | Dog         | Zebra      deriving Eq

twoDifferent a b =
  case a of
    (c1, n1, d1, s1, p1) ->
      case b of
        (c2, n2, d2, s2, p2) ->
          c1 /= c2 && n1 /= n2 && d1 /= d2 && s1 /= s2 && p1 /= p2

allDifferent (p1, p2, p3, p4, p5) =
  twoDifferent p1 p2 && twoDifferent p1 p3 &&
  twoDifferent p1 p4 && twoDifferent p1 p5 &&
  twoDifferent p2 p3 && twoDifferent p2 p4 &&
  twoDifferent p2 p5 && twoDifferent p3 p4 &&
  twoDifferent p3 p5 && twoDifferent p4 p5

anyOfPerson f (p1, p2, p3, p4, p5) =
  f p1 || f p2 || f p3 || f p4 || f p5

anyOfNeighborsPair f (p1, p2, p3, p4, p5) =
  f p1 p2 || f p2 p3 || f p3 p4 || f p4 p5

-- Подсказка 1: На улице стоят пять домов.


-- Англичанин живёт в красном доме
clue02 st =
    anyOfPerson forPerson st
  where
    forPerson (c, n, _, _, _) =
      (n == Englishman) && (c == Red)


-- У испанца есть собака
clue03 st =
    anyOfPerson forPerson st
  where
    forPerson (_, n, _, _, p) =
      (n == Spaniard) && (p == Dog)


-- В зелёном доме пьют кофе
clue04 st =
    anyOfPerson forPerson st
  where
    forPerson (c, _, d, _, _) =
      (c == Green) && (d == Coffee)


-- Украинец пьёт чай
clue05 st =
    anyOfPerson forPerson st
  where
    forPerson (_, n, d, _, _) =
      (n == Ukrainian) && (d == Tea)


-- Зелёный дом стоит сразу справа от белого дома
clue06 st =
    anyOfNeighborsPair forNeighborsPair st
  where
    forNeighborsPair (c1, _, _, _, _) (c2, _, _, _, _) =
      (c1 == Ivory) && (c2 == Green)


-- Тот, кто курит Old Gold, разводит улиток
clue07 st =
    anyOfPerson forPerson st
  where
    forPerson (_, _, _, s, p) =
      (s == OldGold) && (p == Snails)


-- В жёлтом доме курят Kool
clue08 st =
    anyOfPerson forPerson st
  where
    forPerson (c, _, _, s, _) =
      (c == Yellow) && (s == Kools)


-- В центральном доме пьют молоко
clue09 (_, _, (_, _, d, _, _), _, _) =
  d == Milk

-- Норвежец живёт в первом доме
clue10 ((_, n, _, _, _), _, _, _, _) =
  n == Norwegian


-- Сосед того, кто курит Chesterfield, держит лису
clue11 st =
    anyOfNeighborsPair forNeighborsPair st
  where
    forNeighborsPair (_, _, _, s1, p1) (_, _, _, s2, p2) =
      ((s1 == Chesterfield) && (p2 == Fox)) || ((p1 == Fox) && (s2 == Chesterfield))


-- В доме по соседству с тем, в котором держат лошадь, курят Kool
clue12 st =
    anyOfNeighborsPair forNeighborsPair st
  where
    forNeighborsPair (_, _, _, s1, p1) (_, _, _, s2, p2) =
      ((s1 == Kools) && (p2 == Hourse)) || ((p1 == Hourse) && (s2 == Kools))


-- Тот, кто курит Lucky Strike, пьёт апельсиновый сок
clue13 st =
    anyOfPerson forPerson st
  where
    forPerson (_, _, d, s, _) =
      (s == LackyStrike) && (d == OrangeJuice)


-- Японец курит Parliament
clue14 st =
    anyOfPerson forPerson st
  where
    forPerson (_, n, _, s, _) =
      (n == Japanese) && (s == Parliament)


-- Норвежец живёт рядом с синим домом
clue15 st =
    anyOfNeighborsPair forNeighborsPair st
  where
    forNeighborsPair (c1, n1, _, _, _) (c2, n2, _, _, _) =
      ((n1 == Norwegian) && (c2 == Blue)) || ((c1 == Blue) && (n2 == Norwegian))



-- Все характеристики присутствуют
allPresent (p1, p2, p3, p4, p5) =
    forPerson p1 && forPerson p2 && forPerson p3 && forPerson p4 && forPerson p5
  where
    forPerson (c, n, d, s, p) =
      ((c == Yellow   ) || (c == Blue        ) || (c == Red       ) || (c == Ivory      ) || (c == Green     )) &&
      ((n == Norwegian) || (n == Ukrainian   ) || (n == Englishman) || (n == Spaniard   ) || (n == Japanese  )) &&
      ((d == Water    ) || (d == Tea         ) || (d == Milk      ) || (d == OrangeJuice) || (d == Coffee    )) &&
      ((s == Kools    ) || (s == Chesterfield) || (s == OldGold   ) || (s == LackyStrike) || (s == Parliament)) &&
      ((p == Fox      ) || (p == Hourse      ) || (p == Snails    ) || (p == Dog        ) || (p == Zebra     ))


checkState st =
  allDifferent st &&
  clue02 st && clue03 st && clue04 st && clue05 st && clue06 st && clue07 st && clue08 st &&
  clue09 st && clue10 st && clue11 st && clue12 st && clue13 st && clue14 st && clue15 st &&
  allPresent st


-- checkState st =:= True where st free
-- let answer = (
--   (Yellow, Norwegian , Water       , Kools       , Fox   ),
--   (Blue  , Ukrainian , Tea         , Chesterfield, Hourse),
--   (Red   , Englishman, Milk        , OldGold    , Snails),
--   (Ivory , Spaniard  , Orange_juice, Lacky_Strike, Dog   ),
--   (Green , Japanese  , Coffee      , Parliament  , Zebra ))

-- bfs failed
-- dfs succeeded (long time)
