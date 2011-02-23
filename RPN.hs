solveRPN :: String -> String
solveRPN = head . simplify . words

simplify :: [String] -> [String]
simplify eq =
    if isSimplified eq
        then eq
        else simplify $ solveRPN' eq

isSimplified :: [String] -> Bool
isSimplified eq =
    if eq == (solveRPN' eq)
        then True
        else False

solveRPN' :: [String] -> [String]
solveRPN' (x:y:[])   = [x,y]
solveRPN' (x:y:z:[]) = [solveRPNStatement (x, y, z)]
solveRPN' (x:y:z:xs) = [solveRPNStatement (x, y, z)] ++ (solveRPN' xs)
solveRPN' (xs:[])    = [xs]

-- At the moment, these return "<left> <operator> <right>"
-- They should eventually return the result.
-- Example:
--          now:      ("1", "2", "+") -> "1 + 2"
--          expected: ("1", "2", "+") -> "3"
solveRPNStatement :: (String, String, String) -> String
solveRPNStatement (left, right, "+") = left ++ " + " ++ right
solveRPNStatement (left, right, "-") = left ++ " - " ++ right
solveRPNStatement (left, right, "*") = left ++ " * " ++ right
solveRPNStatement (left, right, "/") = left ++ " / " ++ right
solveRPNStatement (left, right, op)  = left ++ " " ++ op ++ "-unknown " ++ right

main = print $ solveRPN "1 2 + 3 -"
