module Clasess where

data Colour = RED | BLUE | GREEN | WHITE | BLACK | RGB Int Int Int deriving (Show)
data UKLength = Yards Double | Feet Double | Inches Double deriving (Show)
data BinTree a = Node a (BinTree a) (BinTree a) | Leaf a | Empty deriving (Eq)

instance Eq Colour where
    (RGB r1 g1 b1) == (RGB r2 g2 b2) = (r1==r2) && (g1==g2) && (b1==b2)
    c1 == c2 = (toRGB c1) == (toRGB c2)

instance Eq UKLength where
    Feet f1 == Feet f2 = f1 == f2
    Yards f1 == Yards f2 = f1 == f2
    Inches f1 == Inches f2 = f1 == f2
    Feet f == i = Feet f == toFeet i
    Yards y == i = Yards y == toYards i
    Inches i == f = Inches i == toInches f


instance Ord UKLength where
    compare f1 f2 = compare (toValue (toFeet f1)) (toValue (toFeet f2))

toValue (Feet n) = n

instance (Show a) => Show (BinTree a) where
    show Empty = "_"
    show (Leaf v) = show v
    show (Node v ti td) = "(" ++ show v ++ " " ++ show ti ++ " " ++  show td ++ ")"

-------------------------------------------

toRGB RED = (RGB 255 0 0)
toRGB GREEN = (RGB 0 255 0)
toRGB BLUE = (RGB 0 0 255)
toRGB BLACK = (RGB 0 0 0)
toRGB WHITE = (RGB 255 255 255)
toRGB (RGB a b c) = (RGB a b c)

toInches (Feet n) = (Inches (n * 12))
toInches (Inches n) = (Inches n)
toInches (Yards n) = (Inches (n*36))

toFeet (Feet n) = (Feet n)
toFeet (Inches n) = (Feet (n/12))
toFeet (Yards n) = (Feet (n*3))

toYards (Feet n) = (Yards (n/3))
toYards (Inches n) = (Yards (n*0.0277778))
toYards (Yards n) = (Yards n)
