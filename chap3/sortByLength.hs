sortByLength [] = []
sortByLength x@(_:[]) = x 
sortByLength xs = let (m, zs) = bubleup xs in (m:sortByLength zs)
				where
					bubleup (x:[])                               = (x, [])
					bubleup (x:xs:zs) | (length x) > (length xs) = let (m, ys) = bubleup (xs:zs) in (m, x:ys)
									  | otherwise                = let (m, ys) = bubleup (x:zs) in (m, xs:ys)
