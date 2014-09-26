data Thing = Operator String | Operand Float

str2float :: String -> Float
str2float str = read str :: Float

showThing :: Thing -> String
showThing (Operator x) = x
showThing (Operand x)  = show x

operator :: String -> Thing
operator x = Operator x

operand :: String -> Thing
operand x = Operand $ str2float x

solveRPN :: String -> String
solveRPN = showThing . head . solveRPN' . thingify . words

thingify :: [String] -> [Thing]
thingify xs = map thingify' xs

thingify' :: String -> Thing
thingify' "+" = operator "+"
thingify' "-" = operator "-"
thingify' "*" = operator "*"
thingify' "/" = operator "/"
thingify' x   = operand x

solveRPN' :: [Thing] -> [Thing]
solveRPN' (x:y:z:xs) = solveRPN' $ solveRPNStatement x y z : xs
solveRPN' (x:y:[])   = [x, y]
solveRPN' (x:[])     = [x]
solveRPN' []         = []

solveRPNStatement :: Thing -> Thing -> Thing -> Thing
solveRPNStatement (Operand l) (Operand r) (Operator op) = Operand $ solveRPNStatement' l r op
solveRPNStatement l r op = error ("Don't know what to do  with " ++ (showThing l) ++ " " ++ (showThing r) ++ " " ++ (showThing op))

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

