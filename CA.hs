module CA where

data CA = Linear (Int, Int, Int) | Nonlinear (Int, Int, Int, Int, Int, Int, Int, Int)

decToBin x =
    let decToBin' 0 = []
        decToBin' y =   let (a,b) = quotRem y 2
                        in  b:(decToBin' a)
    in reverse $ decToBin' x

--mkLinearCA :: Int -> CA
--mkLinearCA rule = 
