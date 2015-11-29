import Util (showTrace)
import qualified Data.Map as M

data R e a = R (e -> a)

instance Functor (R e) where
  fmap f (R x) = R $ \e -> (f . x) e

instance Applicative (R e) where
  pure x = R $ \e -> x
  (R f) <*> (R x) = R $ \e -> (f e) (x e)

instance Monad (R e) where
  return x = R $ \e -> x
  x >>= f  = R $ \e -> runR (f (runR x e)) e

runR :: R e a -> e -> a
runR (R f) e = f e

ask :: R a a
ask = R $ \e -> e

local :: (e -> t) -> R t a -> R e a
local f r = fmap (\e -> runR r (f e)) ask


dothing :: [Int] -> Int
dothing e = head $ showTrace "env: " e

add :: Int -> [Int] -> [Int]
add x = (x:)

go :: Int -> R [Int] ([Int], Int)
go x = do
  v1 <- pure x
  e1 <- ask
  v2 <- local (add 9) (R dothing)
  return (e1, v2)


-- scoping thoughts

-- need to support
-- passing static global scope around                                       -- reader YES
-- passing local scope to certain functions                                 -- reader YES
-- need to update global static scope                                       -- writer YES
-- need to update global static scope from functions that take localscope

data S e a = S (e -> (a, e))

instance Functor (S e) where
  fmap f (S g) = S $ \e -> let (a, newState) = g e
                            in (f a, newState)

instance Applicative (S e) where
  pure x = S $ \e -> (x, e)
  (S f) <*> (S g) = S $ \e -> let (a, newState) = g e
                                  (func, otherNewState) = f newState
                              in  (func a, otherNewState)

instance Monad (S s) where
  return x = S $ \s -> (x,s)
  (S h) >>= f = S $ \s -> let (a, newState) = h s
                              (S g)         = f a
                           in g newState

askS :: S e e
askS = S $ \e -> (e, e)

runS :: S e a -> e -> (a, e)
runS (S f) e = f e
--
-- local :: (e -> t) -> R t a -> R e a
-- local f r = fmap (\e -> runR r (f e)) ask

localS :: (e -> t) -> S t a -> S e a
localS f s = fmap (\e -> fst (runS s (f e))) askS

-- args:
 -- function that modifies the Environment
 -- function that takes modified environment (i.e. localScope), original environment,
   -- and produces a modified version of the original environment
binderS :: (e -> e) -> (e -> S e a) -> S e a
binderS globalToLocal localToResult = S $ \e ->   let local = (globalToLocal e)  -- local == t
                                                      (S g) = (localToResult e)  -- S g == S t a
                                                  in  g local                    -- g local == t -> (a, t)  of

getHead :: S [Int] Int
getHead = S $ \e -> (head e, e)

write :: (e -> e) -> S e ()
write f = S $ \e -> ((), f e)

-- doubleLastBind :: e -> S t a
doubleHeadBind :: [Int] -> S [Int] Int
doubleHeadBind global = S $ \local -> ((head local) * 2, (head local):global)

bind4 :: [Int] -> S [Int] Int
bind4 global = S $ \local -> (head local, 4:global)

-- required to pass:
sc :: Int -> S [Int] Int
sc x = do
  -- v1 <- pure x
  -- v2 <- localS (add 9) getHead   -- local function execution also needs to be able to write up scope changes upwards
  -- v3 <- localS (const 4) askS  -- sets local scope to 4, ask returns
  v4 <- binderS (add 9) bind4
  write ((:)3)
  -- write ((:)5)
  return v4
