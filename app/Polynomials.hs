{-# LANGUAGE OverloadedStrings,OverloadedLists #-}
module Polynomials where

import qualified Data.Map as M
-- 1変数単項式
data Uni = Uni {dim :: Int,comm :: Int}

instance Show Uni where
    show (Uni 0 c) = show c
    show (Uni 1 c) = show c ++ "X"
    show (Uni n c) = show c ++ "X^" ++ show n

instance Semigroup Uni where
    u1 <> u2 = Uni (dim u1 + dim u2) (comm u1 * comm u2)

instance Monoid Uni where
    mempty = Uni 0 1
    mappend = (<>)


-- instance IsString Uni where
--     fromString xs = Uni 1 1 -- あまり筋はよくない。多変数の時にこそ意味を持つ気がする。


data Poly = One Uni | Add Poly Poly | Mul Poly Poly 
-- 文法的にはOneとAddだけで任意の多項式を構成できるが、多項式環としての構造を与えたいのでMulを付与
    -- 多項式の標準形とはこの場合、Mulを含まない式のことと定義できそうだ。
newtype Polyno = P{ unP :: M.Map Int Uni}

instance Show Polyno where
    show (P m) =
        let xs = map snd $ M.toList m
        in concatMap show xs

instance Num Polyno where
    fromInteger n = P $ M.singleton 0 (Uni 0 (fromInteger n))
    p1 + p2 = 
        let p1' = unP p1
            p2' = unP p2
        in P $ M.unionWith (\u1 u2 -> u2{comm = comm u2 + comm u1}) p1' p2'
    p1 * p2 = 
        let p1' = unP p1
            p2' = unP p2
            xs = map snd $ M.toList (unP p1)
        in P $ foldr (\u p -> fmap (u <>) p) p2' xs
    abs p = let p' = unP p in P $ fmap (\u -> u{comm = abs $ comm u}) p'
    negate p = let p' = unP p in P $ fmap (\u -> u{comm = negate $ comm u}) p'
    signum p = 
        let p' = unP p
            m = fst $ M.findMax p'
            u = snd $ M.findMax p'
        in P $ M.singleton m $ Uni m (signum $ comm u)




instance Show Poly where
    show (One u) = show u
    show (Add p1 p2) = "(" ++ show p1 ++ " + " ++ show p2 ++ ")"
    show (Mul p1 p2) = "(" ++ show p1 ++ " * " ++ show p2 ++ ")"

instance Num Poly where
    fromInteger n = One $ Uni 0 (fromInteger n)
    (+) = Add
    (*) = Mul
    abs = undefined -- いずれ改良したいが、評価前の時点で多項式のabsを定義する余地があるか？
    signum = undefined -- 標準形に変換できるようになったら、最高次数の符号を取得して返す関数を作る
    negate = undefined -- Lensとtraversalで全てのOne Uniの係数の符号をnegateする関数にする予定


genFromList_          :: Int -> [Int] -> Poly
genFromList_ n     [] = One $ Uni 0 0
genFromList_ n (x:xs) = (One $ Uni n x) + genFromList_ (n+1) xs -- n=0から始めることで、昇順に多項式を生成する関数

genFromList :: [Int] -> Poly -- 係数リストから昇順の多項式を生成するコンストラクタ
genFromList = genFromList_ 0

eval :: Poly -> Int -> Int
eval (One (Uni d c)) k = c * product (replicate d k)
eval (Add p1 p2) k = eval p1 k + eval p2 k
eval (Mul p1 p2) k = eval p1 k * eval p2 k

