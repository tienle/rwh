intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (h:[]) = h
intersperse x (h:t) =  h ++ x:(intersperse x t)

intersperse' x xs@(h:t) | xs == []  = []
                        | t  == []  = h
                        | otherwise = h ++ x:(intersperse' x t)
