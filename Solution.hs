class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
-- (a -> b) -> [a] -> [b]
instance Fluffy [] where
  furry = map 

-- Exercise 2
-- Relative Difficulty: 1
-- (a -> b) -> Just a -> Just b
instance Fluffy Maybe where
  furry = fmap
  --furry = \fi(Just a)  -> Just $ f a

-- Exercise 3
-- Relative Difficulty: 5
-- (a -> b) -> (t -> a)-> (t -> b) -- func -> func -> func 
-- t -> a -> b --> t->b 
instance Fluffy ((->) t) where
  furry = \f g -> f.g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
-- data Either a b = Left a | Right b
-- (a -> b) -> EitherLeft t a -> EitherLeft t b -- func -> Either a t -> Either b t
instance Fluffy (EitherLeft t) where
  furry = \f (EitherLeft t) -> case t of
                                 (Left  a) -> EitherLeft . Left . f $ a
                                 (Right a) -> EitherLeft . Right $ a

-- Exercise 5
-- Relative Difficulty: 5
-- data Either a b = Left a | Right b
-- (a -> b) -> EitherRight t a -> EitherRight t b -- func -> Either t a -> Either t a
instance Fluffy (EitherRight t) where
  furry = \f (EitherRight t) -> case t of
                                  (Left a)  -> EitherRight . Left $ a
                                  (Right a) -> EitherRight . Right . f $ a
-- It just Monad
class Misty m where
  banana :: (a -> m b) -> m a -> m b -- bind >>=
  unicorn :: a -> m a -- return
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  --
  furry' :: (a -> b) -> m a -> m b -- or (a -> b) -> (m a -> m b) -- func -> monadFunc
  furry' = \f m -> (banana $ unicorn . f)  m
        

-- Exercise 7
-- Relative Difficulty: 2
-- banana :: (a -> [b]) -> [a] -> [b]
instance Misty [] where
  banana  = \f arr -> case arr of
                        (x : xs) -> f x ++ (banana f xs)
                        []       -> []

  unicorn = \x -> [x]


-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana  = \f m -> case m of
                      Just x  -> f x
                      Nothing -> Nothing

  unicorn = \x -> Just x


-- Exercise 9
-- Relative Difficulty: 6
-- banana  :: (a -> t -> b) -> (t -> a) -> (t -> b)
-- unicorn ::  x -> t -> x)
instance Misty ((->) t) where
  banana  = \f m -> \t -> f (m t) t  -- It was hard

  unicorn = const -- or \x -> \t -> x


-- Exercise 10
-- Relative Difficulty: 6
--newtype EitherLeft b a = EitherLeft (Either a b)
--banana :: (a -> m b) -> m a -> m b -- bind >>=
instance Misty (EitherLeft t) where
  banana =  \f e -> case e of
                    (EitherLeft (Left x))  -> f x
                    (EitherLeft (Right x)) -> EitherLeft . Right $ x  -- or otherwise

  unicorn = \x   -> EitherLeft . Left $ x


-- Exercise 11
-- Relative Difficulty: 6
--newtype EitherRight a b = EitherRight (Either a b)
instance Misty (EitherRight t) where
  banana  = \f m -> case m of
                     (EitherRight (Right x)) -> f x
                     (EitherRight (Left x))  ->  EitherRight . Left $ x -- why i can't use q@(EitherRight (Left x)) -> q


  unicorn = \x   -> EitherRight . Right $ x


-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id --- My brain blow up. id :: a -> a | banana :: (a -> m b) -> m a -> m b . So (m a -> m a) -> m (m a) -> m a


-- Exercise 13
-- Relative Difficulty: 6
-- flip furry' :: Misty m => m a -> (a -> b) -> m b
-- how (a - > b) -> m b  to  m (a -> b) -> m b 
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple =  banana . flip furry'


-- Exercise 14
-- Relative Difficulty: 6
-- cheat: sequence $ map f e
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _     = unicorn []
moppy (x:xs) f = banana (\a -> banana  (\b -> unicorn $ a : b) (moppy xs f)) (f x)



-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
-- cheat: sequence
sausage :: (Misty m) => [m a] -> m [a]
sausage ma = moppy ma (id) 


-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
-- without bounus 
--   banana2 f ma mb = banana (\a -> banana (\b -> unicorn $ f a b ) mb ) ma
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = banana (\a -> banana (\b -> unicorn $ f a b ) mb ) ma


-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
-- without bonus
--   banana3 f ma mb mc = banana (\a -> banana (\b -> banana (\c -> unicorn $ f a b c ) mc ) mb  ) ma 
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = banana (\a -> banana (\b -> banana (\c -> unicorn $ f a b c ) mc ) mb  ) ma 


-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
-- without bonus
--  banana4 f ma mb mc md = banana (\a -> banana (\b -> banana (\c-> banana (\d -> unicorn $ f a b c d) md ) mc ) mb ) ma 

banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = banana (\a -> banana (\b -> banana (\c-> banana (\d -> unicorn $ f a b c d) md ) mc ) mb ) ma 

newtype State s a = State {
  state :: (s -> (s, a))
}


-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
-- furry :: (a -> b) -> f a -> f b
--          (a -> b) -> (s -> (s, a)) -> (s -> (s, b)) 
  furry f fa = banana (\a -> unicorn $ f a ) fa


-- ToDo: Fix
-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  -- banana :: (a -> m b) -> m a -> m b 
  banana f ma = State $ \s -> 
    let (s', a)  = state  ma   s 
        (s'', b) = state (f a) s
      in
        (s , b) --- how mappend 
  unicorn a  = State $ \s -> (s, a) 
