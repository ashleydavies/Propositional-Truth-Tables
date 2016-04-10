import Data.List

type Atom        = Char
type Assignment  = (Atom, Bool)
type Situation   = [Assignment]

data Proposition = And      Proposition Proposition
                 | Or       Proposition Proposition
                 | Implies  Proposition Proposition
                 | Not      Proposition
                 | Literal  Atom
                 deriving (Show)
                 
exprToTable = putStrLn . tableify . solve
                 
tableify :: [Situation] -> String 
tableify ss = foldr (\x acc -> x:" | " ++ acc) [] vs
            ++ " expr\n"
            ++ concatMap ((++"  1\n") . tableify') ss
            ++ concatMap ((++"  0\n") . tableify') ss'
    where
        vs = variables $ head ss
        ss' = complement ss
        
        tableify' :: Situation -> String
        tableify' s = concatMap ((:"   ") . charFor s) vs
            
        charFor :: Situation -> Atom -> Char
        charFor s a = toChar (resolve s a)
        
        toChar :: Bool -> Char
        toChar True  = '1'
        toChar False = '0'

solve :: Proposition -> [Situation]
solve (Literal a)           = [ [ (a, True) ] ]
solve (Not prop1)           = complement (solve prop1)
solve (Implies prop1 prop2) = solve (Or (Not prop1) prop2)
solve (And prop1 prop2)     = [mergeSituations x y | x <- (solve prop1),
                                                     y <- (solve prop2),
                                                     situationsAgree x y]
solve (Or  prop1 prop2)     = nubBy situationEqual (expandWith uniqueProp2 s1
                                                 ++ expandWith uniqueProp1 s2)
    where
        s1 = solve prop1
        s2 = solve prop2
        uniqueProp1 = uniqueVariables (safeHead s1 []) (safeHead s2 [])
        uniqueProp2 = uniqueVariables (safeHead s2 []) (safeHead s1 [])
        
safeHead :: [a] -> a -> a
safeHead [] a    = a
safeHead (a:_) _ = a

mergeSituations :: Situation -> Situation -> Situation
mergeSituations a b = (a \\ b) ++ b

situationsAgree :: Situation -> Situation -> Bool
situationsAgree s1 s2 = and $ map (\v -> resolve s1 v == resolve s2 v) vs 
    where
        vs = mutualVariables s1 s2

mutualVariables :: Situation -> Situation -> [Atom]
mutualVariables s1 s2 = intersect (variables s1) (variables s2)

uniqueVariables :: Situation -> Situation -> [Atom]
uniqueVariables s1 s2 = (variables s1) \\ (variables s2)

expandWith :: [Atom] -> [Situation] -> [Situation]
expandWith [] ss     = ss
expandWith (a:as) ss = (map ((a, True) :) rest) ++ (map ((a, False) :) rest) 
    where
        rest = expandWith as ss

complement :: [Situation] -> [Situation]
complement situations = dropSituations (possibleSituations vs) situations
    where
        vs = variables $ head situations
        
dropSituations :: [Situation] -> [Situation] -> [Situation]
dropSituations situations drop = filter (not . situationsContains drop) situations

possibleSituations :: [Atom] -> [Situation]
possibleSituations []     = [[]]
possibleSituations (a:as) = (map ((a, True) :) rest) ++ (map ((a, False) :) rest)
    where
        rest = possibleSituations as
        
situationIn :: Situation -> [Situation] -> Bool
situationIn situation
    = or . map (situationEqual situation)
    
situationsContains :: [Situation] -> Situation -> Bool
situationsContains = flip situationIn

situationEqual :: Situation -> Situation -> Bool
situationEqual sit = and . map (\(c, v) -> resolve sit c == v)

variables :: Situation -> [Atom]
variables = map (\(atom, _) -> atom)

resolve :: Situation -> Char -> Bool
resolve s c = head $ (map (\(_,x) -> x)) $ (filter (\(x,_) -> x == c)) s


instance Num Proposition where
    (+) = Or
    (*) = And
    x - y = Not y

a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z :: Proposition
a = Literal 'a'
b = Literal 'b'
c = Literal 'c'
d = Literal 'd'
e = Literal 'e'
f = Literal 'f'
g = Literal 'g'
h = Literal 'h'
i = Literal 'i'
j = Literal 'j'
k = Literal 'k'
l = Literal 'l'
m = Literal 'm'
n = Literal 'n'
o = Literal 'o'
p = Literal 'p'
q = Literal 'q'
r = Literal 'r'
s = Literal 's'
t = Literal 't'
u = Literal 'u'
v = Literal 'v'
w = Literal 'w'
x = Literal 'x'
y = Literal 'y'
z = Literal 'z'
