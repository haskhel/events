type Vec = Integer -> Bool
-- we could also use: data Vec = Cons Bool Vec

vec_even = f
  where f i = even i

vec_zero = const False

vec_4 4 = True
vec_4 _ = False

pr :: Integer -> Vec -> String
pr n v = map (char.v) [0..n]
  where char True = '1'
        char False = '0'

type Pred = Vec -> Bool

pred_2 v = v 2
pred_4 v = v 4
pred_big v = v 1000

pand :: Pred -> Pred -> Pred
pand p1 p2 v = p1 v && p2 v

por :: Pred -> Pred -> Pred
por p1 p2 v = p1 v || p2 v

pnot :: Pred -> Pred
pnot p1 v = not (p1 v)

peq :: Pred -> Pred -> Pred
peq p1 p2 v = p1 v == p2 v

-- -- --

cons :: Bool -> Vec -> Vec
cons b v = v'
  where v' 0 = b
        v' i = v (i-1)

-- -- --

find :: Pred -> Vec
find p = if p tryFalse then tryFalse else tryTrue
  where tryFalse = cons False $ find (\vec -> p (cons False vec))
        tryTrue  = cons True $ find (\vec -> p (cons True vec))

satisfies :: Pred -> Maybe Vec
satisfies p = if p v then Just v else Nothing
  where v = find p

-- -- --

separates :: Pred -> Pred -> Maybe Vec
separates p1 p2 = satisfies (pnot (peq p1 p2))

equals :: Pred -> Pred -> Bool
equals p1 p2 = case separates p1 p2 of Just _ -> False
                                       Nothing -> True

-- -- --

set i b v = v'
  where v' j | j==i = b
             | otherwise = v j

find_2 :: Pred -> Vec
find_2 p = magic
  where magic k
          | p (set k False magic) = False
          | otherwise             = True
