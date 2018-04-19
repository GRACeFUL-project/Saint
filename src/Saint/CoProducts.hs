{-# LANGUAGE TypeOperators
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , TypeFamilies
#-}
module Saint.CoProducts where

data CoProduct f g a = InL (f a)
                     | InR (g a)

type family f :+: g where
  (CoProduct l r) :+: f = CoProduct l (r :+: f)
  f :+: CoProduct l r   = CoProduct f (l :+: r)
  f :+: g               = CoProduct f g

infixr :+:

{- Need to be explicit about the kind signatures, otherwise GHC infers
 - f :: * -> * -> * -}
class f :< g where
  inject :: f a -> g a

  eject  :: g a -> Maybe (f a)

instance f :< f where
  inject = id

  eject  = Just

{- This instance means its very important to associate to the right
 - when building custom types with :+: -}
instance f :< CoProduct f r where
  inject = InL

  eject (InL f) = Just f
  eject _       = Nothing

instance {-# OVERLAPPABLE #-} f :< r => f :< (CoProduct l r) where
  inject = InR . inject
  
  eject (InR f) = eject f
  eject _       = Nothing
