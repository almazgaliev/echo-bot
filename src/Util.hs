module Util (padLeft, padRight, padRight1, replaceAll, wrapWithTicks) where

{-# INLINE padLeft #-}
padLeft :: a -> Int -> [a] -> [a]
padLeft c n xs = replicate (n - length xs) c ++ xs

{-# INLINE padRight #-}
padRight, padRight1 :: a -> Int -> [a] -> [a]
padRight c n xs = take n $ xs ++ repeat c
padRight1 c n xs = xs ++ replicate (n - length xs) c

commonPrefixLength :: Ord a => [a] -> [a] -> Int
commonPrefixLength a = length . takeWhile (== EQ) . zipWith compare a

replaceAll :: String -> String -> String -> String
replaceAll "" _ _ = ""
replaceAll x a b =
  let
    la = length a
    pl = commonPrefixLength x a
    pl1 = (pl + 1)
    prefix
      | pl == la = b
      | otherwise = take pl1 x
   in
    prefix ++ replaceAll (drop pl1 x) a b


wrapInto :: [a] -> [a] -> [a]
wrapInto s = (s ++) . (++ s)

wrapWithTicks :: String -> String
wrapWithTicks = wrapInto "'"