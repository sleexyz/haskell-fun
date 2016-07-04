-- | Problem: How do we pattern match on things further than just ADT's?

module WadlerViews where


-- | "views as described here specify isomorphisms between datatypes"


data Nat = Z | S Nat
  deriving (Show)

inNat :: Int -> Nat
inNat n 
  | n == 0 = Z
  | n > 0  = S . inNat $ n - 1


outNat :: Nat -> Int
outNat Z = 0
outNat (S n) = outNat n + 1

fib :: Nat ->  Nat
fib Z = Z
fib (S Z) = S Z
fib (S (S n)) = plus (fib n)  (fib (S n))
  where
    plus Z n = n
    plus (S m) n = S (plus m n)


data EO = Zero | Even EO | Odd EO
  deriving (Show)

inEO :: Int -> EO
inEO n
  | n == 0                  = Zero
  | n > 0 && n `mod` 2 == 0 = Even . inEO $ n `div` 2
  | n > 0 && n `mod` 2 == 1 = Odd . inEO $ (n - 1) `div` 2

outEO :: EO -> Int
outEO Zero = 0
outEO (Even n) = 2 * outEO n
outEO (Odd n) = 2 * outEO n + 1


-- | Divide and conquer algorithm for exponentiation:

power :: EO -> EO -> EO
power x m = case m of
  Zero   -> inEO 1
  Even n -> power (inEO ( outEO x * outEO x)) n
  Odd n -> inEO (outEO x * outEO (power  (inEO ( outEO x * outEO x)) n))


data Polar = Polar Float Float
  deriving (Show)
data Cart= Cart Float Float
  deriving (Show)

inC :: Polar -> Cart
inC (Polar r t) = Cart (r * cos t) (r * sin t)

outC :: Cart -> Polar
outC (Cart x y) = Polar (sqrt $ x * x + y * y) (atan2 x y)

add :: Cart -> Cart -> Cart
add (Cart x y) (Cart z w) = Cart (x + z) (y + w)

mult :: Polar -> Polar -> Polar
mult (Polar r a) (Polar s b) = Polar (r * s) (a + b)


-- Have our argument and eat it too!

data As a = As a a
  deriving (Show)

inAs :: a -> As a
inAs x = As x x

outAs :: As a -> a
outAs (As x _) = x

factorial :: As Nat -> As Nat
factorial (As _ Z) =  inAs $ S Z
factorial (As n (S m)) =  inAs $ inNat (outNat n * outNat (outAs (factorial $ inAs m)))

-- Replace view with Predicate!

data ClearInt = EvenP Int | OddP Int



-- viewin

viewcase :: Int -> (Int -> Int) -> Int -> Int
viewcase x s n 
  | n == 0 = x
  | n > 0  = s (n - 1)


zero = 0
suc n = n + 1

fib' m = viewcase zero (\m' -> viewcase (suc zero) (\n -> n + fib' (suc n)) m') m
