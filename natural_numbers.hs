-- Natural numbers representation
data Nat = Zero | Succ Nat deriving Show


-- Recursive operations
int :: Nat -> Int
int Zero = 0
int (Succ n) = 1 + int n

nat :: Int -> Nat
nat 0 = Zero
nat n = Succ (nat (n-1))

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = Succ (add x y)

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul x (Succ y) = add x (mul x y)

pow :: Nat -> Nat -> Nat
pow _ Zero = (Succ Zero)
pow x (Succ y) = mul x (pow x y)

-- Tetration, x `tet` n = x ^ x ^ ... ^ x where there are n xs
tet :: Nat -> Nat -> Nat
tet x Zero = (Succ Zero)
tet x (Succ Zero) = x
tet x (Succ y) = pow x (tet x y)

tet2 :: Nat -> Nat -> Nat
tet2 x Zero = (Succ Zero)
tet2 x (Succ y) = pow x (tet x y)


-- Definitions using folds and unfolds
foldNat :: (a -> a) -> a -> Nat -> a
foldNat c n Zero = n
foldNat c n (Succ x) = c (foldNat c n x)

unfoldNat :: (a -> Bool) -> (a -> a) -> a -> Nat
unfoldNat n d = u
   where u x = if n x then Zero else (Succ (unfoldNat n d (d x)))

int' :: Nat -> Int
int' = foldNat (1+) 0

nat' :: Int -> Nat
nat' = unfoldNat (== 0) (\x -> x-1)

add' :: Nat -> Nat -> Nat
add' x = foldNat (Succ) x

mul' :: Nat -> Nat -> Nat
mul' x = foldNat (add x) Zero

pow' :: Nat -> Nat -> Nat
pow' x = foldNat (mul x) (Succ Zero)

tet' :: Nat -> Nat -> Nat
tet' x = foldNat (pow x) (Succ Zero)


main = do
     print(int (Succ (Succ (Succ Zero))))
     print(int' (Succ (Succ Zero)))
     print(nat 5)
     print(nat' 0)
     print(nat' 3)
     print(add (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(add' (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(mul (Succ Zero) (Succ (Succ Zero)))
     print(mul' (Succ Zero) (Succ (Succ Zero)))
     print(pow (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(pow' (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(tet (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(tet2 (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
     print(tet' (Succ (Succ Zero)) (Succ (Succ (Succ Zero))))
