module Main where

import Lib
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set,singleton,toList,fromList)
import qualified Data.Set as Set(map,empty)
import Data.Maybe(fromMaybe)


main :: IO ()
main = 
    let p = Poly $ M.fromList $ map (\u -> (varData u,u)) [Unit (M.fromList $ zip [1..] $ zipWith VD [1,2,3,4] [2,3,1,6]) 4]
    in print $ p * p * p

-- 変数毎に字数のデータを持たせておく
data VarData = VD{var :: Int, degree :: Int} deriving(Eq,Ord)

instance Show VarData where
    show (VD v d) = "(X" ++ show v ++ " ^ " ++ show d ++ ")"

-- 同じ変数同士の積で次数を変化させる関数
multVarData :: VarData -> VarData -> Maybe VarData
multVarData (VD v1 d1) (VD v2 d2) = 
    if v1 == v2 
        then Just $ VD v1 (d1 + d2)
        else Nothing

type Variables = Map Int VarData

data Unit = Unit{
                varData :: Variables,
                coefficient :: Int
            }deriving (Eq,Ord)

addCoefficients :: Unit -> Unit -> Maybe Unit
addCoefficients (Unit vd1 c1) (Unit vd2 c2) = 
     if vd1 == vd2 
        then Just $ Unit vd1 (c1 + c2)
        else Nothing

multipleUnits :: Unit -> Unit -> Unit
multipleUnits (Unit vd1 c1) (Unit vd2 c2) =
    let vd = M.unionWith (\x y -> fromMaybe (VD 0 0) $ multVarData x y) vd1 vd2
        c  = c1 * c2
    in Unit vd c

instance Show Unit where
    show (Unit vd c) = 
        let vs = mconcat . map snd . M.toList $ fmap show vd
        in show c ++ " * " ++ vs


newtype Poly = Poly {unPoly :: Map Variables Unit} deriving(Eq,Ord,Show)

multiplePoly :: Poly -> Poly -> Poly
multiplePoly p1 p2 =
    let l1 = map snd . M.toList . unPoly $ p1
        l2 = map snd . M.toList . unPoly $ p2
        l  = map (\u -> (varData u,u)) $ multipleUnits <$> l1 <*> l2
        reduction :: [(Variables,Unit)] -> Poly -> Poly
        reduction [] p     = p
        reduction (x:xs) p = 
            let vs = fst x
                u  = snd x
                addKeisuu x y = fromMaybe (Unit M.empty 0) $ addCoefficients x y
                p' = Poly . M.insertWith addKeisuu vs u $ unPoly p
            in reduction xs p'
    in reduction l (Poly M.empty)

instance Num Poly where
    fromInteger n = Poly $ M.singleton  M.empty (Unit M.empty $ fromIntegral n) -- 定数項は0次元
    p1 + p2 = Poly $ M.unionWith (\x y -> fromMaybe (Unit M.empty 0) $ addCoefficients x y) (unPoly p1) (unPoly p2)
    p1 * p2 = multiplePoly p1 p2
 

