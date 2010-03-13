data Tree a = Node a (Tree a) (Tree a)
            | Empty
			  deriving (Show)

treeHeight :: (Num b, Ord b) => Tree a -> b
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r) 
treeHeight _ = 0
