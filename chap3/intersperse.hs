intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (h:[]) = h
intersperse x (h:t) =  h ++ x:(intersperse x t)

intersperse' x [] = []
intersperse' x (h:t)    | t  == []  = h
                        | otherwise = h ++ x:(intersperse' x t)
