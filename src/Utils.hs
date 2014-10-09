module Utils where

-- | Replace the element of a list at the given position with a new
-- value.
replace :: [a] -> Int -> a -> Maybe [a]
replace xs n x
  | n >= length xs = Nothing
  | otherwise =
      Just (hd ++ [x] ++ tail tl)
        where
          (hd, tl) = splitAt n xs

-- | Remove the element from the given position in a list.
remove :: Int -> [a] -> Maybe [a]
remove n xs
  | n >= length xs = Nothing
  | otherwise =
      Just (hd ++ tail tl)
        where
          (hd, tl) = splitAt n xs

-- | Index into a list without calling @error@ on out-of-bounds indices.
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ []     = Nothing
safeIndex 0 (x:_)  = Just x
safeIndex n (_:xs) = safeIndex (n - 1) xs
