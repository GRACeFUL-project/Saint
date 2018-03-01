{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , TypeApplications
#-}
module Example where

import Saint

ifE :: Bool -> Int -> Int -> Int 
ifE b x y = if b then x else y

lib :: (A0 Bool :< t, FullType (Type t)) => Library t
lib = Library "If else"
        [ Item "ifE"   ( ifE   ::: bool --> int --> int --> int)
        , Item "zero"  ( 0     ::: int)
        , Item "one"   ( 1     ::: int)
        , Item "True"  ( True  ::: bool)
        , Item "False" ( False ::: bool)
        ]

listFunctions :: (A1 [] :< t, A0 Bool :< t, FullType (Type t)) => Library t
listFunctions = Library "List functions"
                  [ Item "range"   ((\a b -> [a .. b]) ::: int --> int --> list int)
                  , Item "map"     (map                ::: (int --> int) --> list int --> list int)
                  , Item "reverse" (reverse            ::: list int --> list int) 
                  , Item "zero"    (0                  ::: int)
                  , Item "ten"     (10                 ::: int)
                  , Item "suc"     ((+1)               ::: int --> int)
                  ]

e1 :: Either String Int
e1 = run @(A0 Int :+: A0 Bool) int lib "(\\x . ifE x one zero) True"

e2 :: Either String [Int]
e2 = run @(A0 Int :+: A0 Bool :+: A1 []) (list int) listFunctions "map (\\x. suc x) (range zero ten)"

e3 :: Either String [Int]
e3 = run @(A0 Int :+: A0 Bool :+: A1 []) (list int) listFunctions "map (\\x. suc (suc x)) (range zero (suc ten))"

e4 :: Either String [Int]
e4 = run @(A0 Int :+: A0 Bool :+: A1 []) (list int) listFunctions "map (\\x. x) (range 0 10)"
