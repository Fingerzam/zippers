type Zipper = ([Integer], Integer, [Integer])

open :: [Integer] -> Maybe Zipper
open []     = Nothing
open (x:xs) = Just ([], x, xs)

close :: Zipper -> [Integer]
close (prefix, current, suffix) = (reverse prefix) ++ [current] ++ suffix

forward :: Zipper -> Maybe Zipper
forward (_,      _,       []         ) = Nothing
forward (prefix, current, next:suffix) = Just (current : prefix, next, suffix)

backward :: Zipper -> Maybe Zipper
backward ([], _, _)                     = Nothing
backward (prefix, current, next:suffix) = Just (current : prefix, next, suffix)

update :: Integer -> Zipper -> Zipper
update x (prefix, _, suffix) = (prefix, x, suffix)
