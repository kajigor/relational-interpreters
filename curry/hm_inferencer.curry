data Num = Z
         | S Num
         deriving Eq

data Tuple a b c = Tuple a b c

data Lambda_data a = TInt
                   | TBool
                   | TPair (Lambda_data a) (Lambda_data a)
                   | TVar a
                   | TFun (Lambda_data a) (Lambda_data a)

data Gen_data a = Gen [a] (Lambda_data a)

data Literal = LInt Int
             | LBool Bool

data Lambda a = Var_  a
              | Lit Literal
              | Tuple2 (Lambda a) (Lambda a)
              | App (Lambda a) (Lambda a)
              | Abst a (Lambda a)
              | Let_  a (Lambda a) (Lambda a)

------------------------------------------------------------

remove e l =
  case l of
    [] -> []
    (e' : l') ->
      if e == e' then remove e l' else (e' : remove e l')

remove_list rem l =
  case rem of
    []        -> l
    x : rem' -> remove_list rem' (remove x l)

remove_repeats l =
  case l of
    []       -> []
    e' : l' -> e' : remove_repeats (remove e' l')

append a b =
  case a of
    [] -> b
    (x : a') -> (x : append a' b)

free_vars typ =
    remove_repeats (fvars typ)
  where
    fvars typ =
      case typ of
        TInt -> []
        TBool -> []
        TVar v -> [v]
        TPair a b -> append (fvars a) (fvars b)
        TFun  a b -> append (fvars a) (fvars b)

gen_free_vars gtyp =
  case gtyp of
    Gen vars t -> remove_list vars (free_vars t)

env_free_vars env =
    remove_repeats (get_fvs env)
  where
    get_fvs env =
      case env of
        [] -> []
        (p : env') ->
          case p of
            (v, gtyp) -> append (gen_free_vars gtyp) (get_fvs env')

------------------------------------------------------------

lookup v s =
  case s of
    (p : xs) ->
      case p of
        (v', t) ->
          if v == v' then t else lookup v xs

lookup_total v s =
  case s of
    []      -> Nothing
    (p : xs) ->
      case p of
        (v', t) ->
          if v == v' then Just t else lookup_total v xs

------------------------------------------------------------

delete e l =
  case l of
    [] -> l
    (p : l') ->
      case p of
        (e', t) ->
          if e == e' then delete e l' else p : delete e l'

delete_list del l =
  case del of
    [] -> l
    (e : del') -> delete_list del' (delete e l)

map f l =
  case l of
    [] -> []
    (x : l') -> (f x : map f l')

------------------------------------------------------------

mb_bind a f = f a

------------------------------------------------------------

-- Substitute all data variables if their instantiation is already known
apply s typ =
  case typ of
    TInt -> typ
    TBool -> typ
    TPair a b -> TPair (apply s a) (apply s b)
    TFun a b -> TFun (apply s a) (apply s b)
    TVar v ->
      case lookup_total v s of
        Just typ' -> typ'
        Nothing -> typ

------------------------------------------------------------

gen_apply s gtyp =
  case gtyp of
    Gen vars typ -> Gen vars (apply (delete_list vars s) typ)

------------------------------------------------------------

env_apply s env =
    map member_apply env
  where
    member_apply p =
      case p of
        (v, gtyp) -> (v, gen_apply s gtyp)

------------------------------------------------------------

compose_subst s1 s2 =
    append (map apply_member s2) s1
  where
    apply_member p =
      case p of
        (k, v) -> (k, apply s1 v)

------------------------------------------------------------

generalize env typ =
  let vars = remove_list (env_free_vars env) (free_vars typ) in
  Gen vars typ

------------------------------------------------------------


replace v subst typ =
  case typ of
    TInt -> typ
    TBool -> typ
    TPair a b -> TPair (replace v subst a) (replace v subst b)
    TFun a b -> TFun (replace v subst a) (replace v subst b)
    TVar v' -> if v == v' then subst else typ

------------------------------------------------------------

instantiate fv fresher gtyp =
    case gtyp of
      Gen vars typ -> inst fv vars typ
  where
    inst fv vars typ =
      case vars of
        [] -> (typ, fv)
        (v : vars') ->
          let new_typ = replace v (TVar fv) typ
          in inst (fresher fv) vars' new_typ

------------------------------------------------------------

has_var t v =
  case t of
    TInt -> False
    TBool -> False
    TVar v' -> v == v'
    TPair a b -> if has_var a v then True else has_var b v
    TFun a b -> if has_var a v then True else has_var b v

------------------------------------------------------------


var_bind v t =
  let has = has_var t v in
  case t of
    TInt -> [(v, t)]
    TBool -> [(v, t)]
    TVar v' -> if has then [] else [(v, t)]
    TPair a b ->
      case has of
        False -> [(v, t)]
    TFun a b ->
      case has of
        False -> [(v, t)]

------------------------------------------------------------

mgu t1 t2 =
  case t1 of
    TInt ->
      case t2 of
        TInt -> []
        TVar v -> var_bind v t1
    TBool ->
      case t2 of
        TBool -> []
        TVar v -> var_bind v t1
    TVar v -> var_bind v t2
    TFun a b ->
      case t2 of
        TVar v -> var_bind v t1
        TFun a' b' ->
          let s1 = mgu a a' in
          let s2 = mb_bind s1 (\s -> mgu (apply s b) (apply s b')) in
          compose_subst s1 s2
    TPair a b ->
      case t2 of
        TVar v -> var_bind v t1
        TPair a' b' ->
          let s1 = mgu a a' in
          let s2 = mb_bind s1 (\s -> mgu (apply s b) (apply s b')) in
          mb_bind s1 (\x -> mb_bind s2 (\y -> compose_subst x y))

------------------------------------------------------------

ti_literal l =
  case l of
    LInt i -> TInt
    LBool b -> TBool

------------------------------------------------------------

-- [fv] is the next available name from new data variable
-- [fresher] constructs _next_ available name for new data variable
-- [env] is a mapping from varibles-terms to its datas
-- Returns [(env * typ * next_fv) maybe] where
--   typ is an answer
--   next_fv is next name for data variable
--   env is a mapping from data variables to datas
ti fv fresher env term =
  let ret s t fv = Tuple s t fv in

  case term of
    Var_ v ->
      case lookup v env of
        gt ->
          let pair0 = instantiate fv fresher gt in
          case pair0 of
            (t, fv') -> ret [] t fv'
    Lit l -> ret [] (ti_literal l) fv
    Abst v b ->
      let gt = Gen [] (TVar fv) in
      let env' = (v, gt) : env in
      let fv' = fresher fv in
      let tuple0 = ti fv' fresher env' b in
      mb_bind tuple0
      (\tpl ->
        case tpl of
          Tuple s t fv'' ->
            ret s (TFun (apply s (TVar fv)) t) fv''
      )
    Tuple2 l r ->
      let fv' = fresher fv in
      let tuple1 = ti fv' fresher env l in
      mb_bind tuple1
      (\tpl1 ->
          case tpl1 of
            Tuple s1 t1 fv'' ->
              let tuple2 = ti fv'' fresher (env_apply s1 env) r in
              mb_bind tuple2
              (\tpl2 ->
                case tpl2 of
                  Tuple s2 t2 fv''' ->
                    ret (compose_subst s1 s2) (TPair t1 t2) fv'''
              )
      )
    App f a ->
      let fv' = fresher fv in
      let tuple1 = ti fv' fresher env f in
      mb_bind tuple1
      (\tpl1 ->
        case tpl1 of
          Tuple s1 t1 fv'' ->
            let tuple2 = ti fv'' fresher (env_apply s1 env) a in
            mb_bind tuple2
            (\tpl2 ->
                case tpl2 of
                  Tuple s2 t2 fv''' ->
                    let subst = mgu (apply s2 t1) (TFun t2 (TVar fv)) in
                    mb_bind subst
                    (\s3 ->
                      let s = compose_subst (compose_subst s1 s2) s3 in
                      ret s (apply s3 (TVar fv)) fv'''
                    )
              )
      )
    Let_ v a b ->
      let tuple1 = ti fv fresher env a in
      mb_bind tuple1
      (\tpl1 ->
        case tpl1 of
          Tuple s1 t1 fv' ->
            let gt1 = generalize (env_apply s1 env) t1 in
            let env' = (v, gt1) : env in
            let tuple2 = ti fv' fresher env' b in
            mb_bind tuple2
            (\tpl2 ->
              case tpl2 of
                Tuple s2 t2 fv'' ->
                  let s = compose_subst s1 s2 in
                  ret s t2 fv''
            )
      )

------------------------------------------------------------


data_inference first_var fresher term =
 let tuple0 = ti first_var fresher [] term in
  mb_bind tuple0
  (\tpl ->
    case tpl of
      Tuple s t fv -> apply s t
  )

------------------------------------------------------------

nat_data_inference term = data_inference Z (\n -> S n) term