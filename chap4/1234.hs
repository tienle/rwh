safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail (_:[]) = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast x  = case x of
                [] -> Nothing
                (a:[]) -> Just a
                (y:ys) -> safeLast ys

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just (safeInit' x)
             where safeInit' (x:[]) = []
                   safeInit' (x:xs) = x:safeInit' xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs = let (e, r) = break f xs  
                 in e:splitWith f (tail r)












