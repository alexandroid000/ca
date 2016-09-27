module CA where

import Data.Tuple.Select

data CAstate = O | I
    deriving (Show)

infixl 6 .-
infixl 6 .+

(.+) :: CAstate -> CAstate -> CAstate
O .+ a = a
a .+ O = a
I .+ I = O

(.-) :: CAstate -> CAstate -> CAstate
(.-) = (.+)

(.*) :: CAstate -> CAstate -> CAstate
O .* _ = O
_ .* O = O
I .* _ = I

type Neighborhood = (CAstate, CAstate, CAstate)

data CA = Linear (Neighborhood -> CAstate) | Nonlinear [CAstate]

instance Show CA where
    show (Nonlinear is) = show is
    show (Linear f) =
        let alpha = show (f (I,O,O) .- f (O,O,O))
            beta = show (f (O,I,O) .- f (O,O,O))
            gamma = show (f (O,O,I) .- f (O,O,O))
            v = show (f (O,O,O))
        in  alpha++"*q + "++beta++"*r + "++gamma++"*s + "++v

-- Helper Functions
-------------------

-- mathematically, can only contain 0s and 1s
-- but not specifying that with types quite yet
decToBin :: Int -> [Int]
decToBin x =
    let decToBin' 0 = [0]
        decToBin' 1 = [1]
        decToBin' y =   let (a,b) = quotRem y 2
                        in  b:(decToBin' a)
    in reverse $ decToBin' x

binToDec :: [Int] -> Int
binToDec xs =
    let binToDec' []        = 0
        binToDec' (a:as)    = a + 2*(binToDec' as)
    in  binToDec' $ reverse xs

-- Constructing CA
------------------

binToCA :: [Int] -> [CAstate]
binToCA [] = []
binToCA (a:as) = case a of
                    0 -> O:(binToCA as)
                    1 -> I:(binToCA as)
                    _ -> binToCA as

csToBin :: [CAstate] -> [Int]
csToBin [] = []
csToBin (a:as) = case a of
                    O -> 0:(csToBin as)
                    I -> 1:(csToBin as)

caToTable :: CA -> [CAstate]
caToTable ca = rule
    where (Nonlinear rule) = caToNonlinear ca

caToDec :: CA -> Int
caToDec = binToDec . csToBin . caToTable


-- only implemented for 8-state configurations
mkNonlinCA :: Int -> CA
mkNonlinCA num =
    let binRule = binToCA $ decToBin num
    in Nonlinear (pad 8 binRule)

pad :: Int -> [CAstate] -> [CAstate]
pad i brule
    | length brule < i = pad i (O:brule)
    | otherwise = brule

-- only implemented for 3-neighborhoods
mkLinearCA :: Int -> CA
mkLinearCA rule =
    let nlCA = mkNonlinCA rule
        f (q,r,s) =   ( (nlCA `atNbrhd` (q,O,O)) .+
                        (nlCA `atNbrhd` (O,r,O)) .+
                        (nlCA `atNbrhd` (O,O,s)))
    in Linear f

caToNonlinear :: CA -> CA
caToNonlinear ca =
    let mk3 (a:b:c:xs) = (a,b,c)
        nbrhoods = map (mk3 . pad 3 . binToCA . decToBin) [7,6..0]
        rule = map (atNbrhd ca) nbrhoods
    in  Nonlinear rule

-- why is CA notation reversed by default?
atNbrhd :: CA -> Neighborhood -> CAstate
atNbrhd (Nonlinear ca) n@(p,q,r) =
    let index = binToDec $ csToBin [p,q,r]
        weirdReverse = reverse ca
    in  weirdReverse !! index
atNbrhd (Linear f) nbrhood = f nbrhood

algebraToNon :: (Neighborhood -> CAstate) -> CA
algebraToNon f = caToNonlinear $ Linear f

-- only implemented for 3-neighborhoods
mkAlgebraic :: CA -> (Neighborhood -> CAstate)
mkAlgebraic (Linear f) = \(q,r,s) ->
    ( (f (I,O,O) .- f (O,O,O)).*q
    .+ (f (O,I,O) .- f (O,O,O)).*r
    .+ (f (O,O,I) .- f (O,O,O)).*s
    .+ f (O,O,O) )
