import Control.Lens

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing lleft _      op (Left l)  = fmap Left $ lleft op l
choosing _     lright op (Right r) = fmap Right $ lright op r

example1, example2 :: Either (Int, Char) (Bool, Int)
example1 = Left (1,'a')
example2 = Right (True,2)

_all :: Eq a => a -> Lens [a] [a] a a
_all ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s
