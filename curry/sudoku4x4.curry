data Num = N1 | N2 | N3 | N4

data Sudoku4X4 =
  S4x4 Num Num Num Num Num Num Num Num Num Num Num Num Num Num Num Num

not_eq a b =
  case a of
    N1 -> (case b of N1 -> False ; N2 -> True  ; N3 -> True  ; N4 -> True )
    N2 -> (case b of N1 -> True  ; N2 -> False ; N3 -> True  ; N4 -> True )
    N3 -> (case b of N1 -> True  ; N2 -> True  ; N3 -> False ; N4 -> True )
    N4 -> (case b of N1 -> True  ; N2 -> True  ; N3 -> True  ; N4 -> False)

all_different a b c d =
  not_eq a b && not_eq a c && not_eq a d && not_eq b c && not_eq b d && not_eq c d

check_sudoku sudoku =
  case sudoku of
    S4x4 a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44 ->
      all_different a11 a12 a13 a14 &&
      all_different a21 a22 a23 a24 &&
      all_different a31 a32 a33 a34 &&
      all_different a41 a42 a43 a44 &&
      all_different a11 a21 a31 a41 &&
      all_different a12 a22 a32 a42 &&
      all_different a13 a23 a33 a43 &&
      all_different a14 a24 a34 a44 &&
      all_different a11 a12 a21 a22 &&
      all_different a13 a14 a23 a24 &&
      all_different a31 a32 a41 a42 &&
      all_different a33 a34 a43 a44