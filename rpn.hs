import Text.Read
import Data.Maybe (maybe)

data Thing = Operator String | Operand Float

instance Show Thing where
  show (Operator x) = x
  show (Operand x)  = show x

solveRPN :: String -> String
solveRPN = show . head . solveRPN' . thingify . words

thingify :: [String] -> [Thing]
thingify = map thingify'

thingify' :: String -> Thing
thingify' str = maybe (Operator str) Operand f
  where
    f = readMaybe str :: Maybe Float

solveRPN' :: [Thing] -> [Thing]
solveRPN' (x:y:z:xs) = solveRPN' $ solveRPNStatement x y z : xs
solveRPN' xs = xs

solveRPNStatement :: Thing -> Thing -> Thing -> Thing
solveRPNStatement (Operand l) (Operand r) (Operator op) = Operand $ solveRPNStatement' l r op
solveRPNStatement l r op = error ("Don't know what to do  with " ++ (show l) ++ " " ++ (show r) ++ " " ++ (show op))

solveRPNStatement' :: Float -> Float -> String -> Float
solveRPNStatement' l r "+" = l + r
solveRPNStatement' l r "-" = l - r
solveRPNStatement' l r "*" = l * r
solveRPNStatement' l r "/" = l / r
solveRPNStatement' l r op  = error ("Unknown operator: " ++ op)

main = do
    print $ solveRPN "1 2 + 3 -"
    print $ solveRPN "1 2 *"
    print $ solveRPN "1 2 x"

