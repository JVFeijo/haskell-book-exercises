{-# LANGUAGE InstanceSigs #-}
import System.Random
import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Tuple (swap)

-- Six-sided die
data Die = DieOne
           | DieTwo
           | DieThree
           | DieFour
           | DieFive
           | DieSix
 deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
               1 -> DieOne
               2 -> DieTwo
               3 -> DieThree
               4 -> DieFour
               5 -> DieFive
               6 -> DieSix
               x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: State StdGen Die
rollDie = state $ do
                    (n, s) <- randomR (1, 6)
                    return (intToDie n, s)

rollInt :: State StdGen Int
rollInt = state (randomR (1, 6))

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
                     where go :: Int -> Int -> StdGen -> Int
                           go sum count gen | sum >= 20 = count
                                            | otherwise = let (die, nextGen) = randomR (1, 6) gen in go (sum + die) (count + 1) nextGen

rollsToGetN :: StdGen -> Int -> Int
rollsToGetN g n = go n 0 g
                     where go :: Int -> Int -> StdGen -> Int
                           go sum count gen | sum <= 0 = count
                                            | otherwise = let (die, nextGen) = randomR (1, 6) gen in go (sum - die) (count + 1) nextGen

takeWhileN :: Int -> [Int] -> [Int]
takeWhileN n (x:xs) | n <= 0 = []
takeWhileN n (x:xs) | n > 0 = x:(takeWhileN (n-x) xs)

rollsToGetN' :: Int -> StdGen -> (Int, [Die])
rollsToGetN' n g = evalState (((,) <$> length <*> (map intToDie)) <$> (takeWhileN n) <$> (sequenceA (repeat rollInt))) g

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
 fmap :: (a -> b) -> Moi s a -> Moi s b
 fmap f (Moi g) = Moi $ ((,) <$> (f . fst) <*> snd) <$> g


instance Applicative (Moi s) where
 pure :: a -> Moi s a
 pure a = Moi $ (\s -> (a, s))
 
 (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
 (Moi f) <*> (Moi g) = Moi $ (\s1 -> let (x1, s2) = g s1
                                         (f2, s3) = f s2
                                     in ((f2 x1), s3))


appMoi :: Moi s (a -> b) -> Moi s a -> Moi s b
appMoi (Moi f) (Moi g) = Moi $ ((\(x1, s2) -> ((\(f2, s3) -> (f2 x1, s3)) <$> f) s2 ) <$> g)

instance Monad (Moi s) where
 return = pure

 (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
 (Moi f) >>= g = Moi $ (\s1 -> let (x1, s2) = f s1
                                   moi2 = g x1
                               in  runMoi moi2 s2)


get' :: Moi s s
get' = Moi $ \s -> (s, s)

put' :: s -> Moi s ()
put' s = Moi $ \_ -> ((), s)

exec' :: Moi s a -> s -> s
exec' (Moi sa) s = (snd . sa) s

eval' :: Moi s a -> s -> a
eval' (Moi sa) s = (fst . sa) s

modifyy :: (s -> s) -> Moi s ()
modifyy = \f -> Moi $ \s -> ((), f s)
