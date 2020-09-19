{-# LANGUAGE OverloadedStrings #-}
module Polynomials where

-- 1変数単項式
data Uni = Uni {dim :: Int,comm :: Int}

instance Show Uni where
    show (Uni 0 c) = show c
    show (Uni 1 c) = show c ++ "X"
    show (Uni n c) = show c ++ "X^" ++ show n

instance Monoid Uni where
    mempty = Uni 0 1
    mappend (Uni d1 c1) (Uni d2 c2) = Uni (d1 + d2) (c1 * c2)

-- instance IsString Uni where
--     fromString xs = Uni 1 1 -- あまり筋はよくない。多変数の時にこそ意味を持つ気がする。

data Poly = One Uni | Add Poly Poly | Mul Poly Poly

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