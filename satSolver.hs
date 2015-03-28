module satSolver where
import Data.List
import Solver

data BExp = BConst Bool | Var String | And BExp BExp | Not BExp | Or BExp BExp deriving (Eq,Show)

data SatConfig = SatConfig BExp [(String, Bool)] [String] deriving Eq

instance Show SatConfig where
	show (SatConfig _ x _) = show x

instance  Config SatConfig where
	successors (SatConfig _ _ []) = []
	successors (SatConfig exp val list@(x:xs)) = [SatConfig exp (val ++ [(x,v)]) xs | v <- [True, False]]

satIsGoal:: [String] -> SatConfig -> Bool
satIsGoal vars (SatConfig exp val _) = fetchBoolForAll vars val && satisfy exp val
--satIsGoal ["a","b"] (SatConfig (Or (Var "a") (Var "b")) [("a",True),("b", True)] [])

fetchBoolForAll :: [String] -> [(String, Bool)] -> Bool
fetchBoolForAll [] _ = True
fetchBoolForAll (x:xs) y = case lookup x y of
							Just n -> fetchBoolForAll xs y
							Nothing -> False				 
-- fetchBoolForAll ["a","b"] [("a",False),("b",False)]

satisfy :: BExp -> [(String, Bool)] -> Bool
satisfy (BConst a) x = a
satisfy (Var a) x = (fetchBool a x)
satisfy (And a b) x = (satisfy a x) && (satisfy b x)
satisfy (Or a b) x = (satisfy a x) || (satisfy b x)
satisfy (Not a) x = not (satisfy a x)
-- satisfy (Or (And (Var "c") (Var "d")) (Var "d")) [("d",True),("c",True)] 

fetchBool :: String -> [(String, Bool)] -> Bool
fetchBool a x = case lookup a x of
                Nothing -> error ("undefined variable: " ++ (show a))
                Just n  -> n
--fetchBool "a" [("a",True)]

getVars :: BExp -> [String]
getVars (BConst a) = []
getVars (Var a) = [a]
getVars (And a b) = (getVars a ) ++ (getVars b )
getVars (Or a b) = (getVars a ) ++ (getVars b )
getVars (Not a) = (getVars a)
-- getVars (Or (And (Var "c") (Var "d")) (Var "e"))

satSolve :: (BExp) -> (Maybe SatConfig)
satSolve exp = 
  let sigma      = getVars exp
      isGoal     = (satIsGoal sigma)
      initConfig = (SatConfig exp [] sigma)
  in solve isGoal initConfig

