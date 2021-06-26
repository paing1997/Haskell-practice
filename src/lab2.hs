module Lab2 where


--binary search tree

data (Tree a) = Empty | Node (Tree a) a (Tree a) deriving (Show)

instance Eq a => Eq (Tree a) where 
   Empty == Empty = True
   tree1 == tree2 = toList tree1 == toList tree2

instance Ord a => Semigroup (Tree a) where
    (<>) a b = foldr (flip insertToTree) a b

instance Ord a => Monoid (Tree a) where
    mempty = Empty

instance Functor Tree where
    fmap _ Empty               = Empty
    fmap f (Node left v right) = Node (fmap f left) (f v) (fmap f right)

instance Foldable Tree where
    foldr _ init Empty    = init
    foldr f init (Node left v right) = foldr f (f v (foldr f init left)) right





insertToTree Empty value = Node Empty value Empty
insertToTree (Node left node_value right) value | node_value == value = Node left value right
                                                | node_value < value  = Node left node_value (insertToTree right value)
                                                | otherwise           = Node (insertToTree left value) node_value right


deleteFromTree:: (Ord a) => Tree a -> a -> Tree a
deleteFromTree Empty _ = Empty
deleteFromTree node@(Node left node_value right) value | node_value == value = condenceTree node
                                                      | node_value > value  = Node (deleteFromTree left value) node_value right
                                                      | otherwise           = Node left node_value (deleteFromTree right value)



treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ Empty                       = Empty
treeFilter predicate node@(Node left v right) | predicate v = Node (treeFilter predicate left) v (treeFilter predicate right)
                                              | otherwise        = treeFilter predicate $ condenceTree node


--Utils

condenceTree (Node Empty _ Empty) = Empty
condenceTree (Node Empty _ right) = right
condenceTree (Node left _ Empty)  = left
condenceTree (Node left _ right)  = let (left', newValue) = getGreatest left in Node left' newValue right
getGreatest (Node Empty x Empty) = (Empty, x)
getGreatest (Node left x Empty)  = (left, x)
getGreatest (Node left x right)  = let (right', val) = getGreatest right in (Node left x right', val)

toList :: Tree a -> [a]
toList Empty = []
toList (Node left v right) = toList left ++ [v] ++ toList right