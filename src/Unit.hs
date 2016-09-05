module Unit where

newtype Unit = Unit Integer
               deriving (Eq, Ord, Show)

instance Num Unit where
  fromInteger = Unit
  negate (Unit x) = Unit (-x)
  (Unit x) + (Unit y) = Unit (x + y)
  (Unit x) * (Unit y) = Unit (x * y)
  signum (Unit x) = Unit $ signum x
  abs (Unit x) = Unit (abs x)
  
instance Enum Unit where
  toEnum a          = Unit (fromIntegral a)
  fromEnum (Unit a) = fromInteger a
  
u0 :: Unit
u0 = Unit 0