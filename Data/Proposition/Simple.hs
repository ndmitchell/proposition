{-|
    Proposition that focuses on being correct,
    and working as a test harness for comparisons.
    
    Some basic simplifications are performed, such as @True || x = x@.
-}

module Data.Proposition.Simple(PropSimple, propRebuildSimple) where

import Data.Proposition.Internal
import Control.Monad
import Data.List


propRebuildSimple :: (Prop p, PropLit a) => p a -> PropSimple a
propRebuildSimple = propRebuild


data PropSimple a = Lit a
                  | Not (PropSimple a)
                  | And [PropSimple a]
                  | Or  [PropSimple a]
                  deriving Eq


instance Show a => Show (PropSimple a) where
    show (Lit a ) = show a
    show (Not x ) = "~(" ++ show x ++ ")"
    show (And []) = "True"
    show (And xs) = "(" ++ concat (intersperse " ^ " (map show xs)) ++ ")"
    show (Or  []) = "False"
    show (Or  xs) = "(" ++ concat (intersperse " v " (map show xs)) ++ ")"


instance Prop PropSimple where
    propTrue = And []
    propFalse = Or []
    
    propIsTrue (Lit _ ) = False
    propIsTrue (Not x ) = propIsFalse x
    propIsTrue (And xs) = all propIsTrue xs
    propIsTrue (Or  xs) = any propIsTrue xs
    
    propIsFalse (Lit _ ) = False
    propIsFalse (Not x ) = propIsTrue x
    propIsFalse (And xs) = any propIsFalse xs
    propIsFalse (Or  xs) = all propIsFalse xs
    
    propLit = Lit
    propAnd a b = propAnds [a,b]
    propOr  a b = propOrs  [a,b]
    
    propNot (Not a) = a
    propNot a = Not a

    propAnds xs | any propIsFalse xs = propFalse
                | otherwise = case filter (not . propIsTrue ) xs of
                                   [x] -> x
                                   xs -> And xs
    
    propOrs  xs | any propIsTrue  xs = propTrue
                | otherwise = case filter (not . propIsFalse) xs of
                                   [x] -> x
                                   xs -> Or xs

    propMapM f (Lit a ) = f a
    propMapM f (Not a ) = liftM propNot  $ propMapM f a
    propMapM f (And xs) = liftM propAnds $ mapM (propMapM f) xs
    propMapM f (Or  xs) = liftM propOrs  $ mapM (propMapM f) xs
    
    propFold fs (Lit a ) = foldLit fs a
    propFold fs (Not a ) = foldNot fs (propFold fs a)
    propFold fs (And xs) = foldAnd fs (map (propFold fs) xs)
    propFold fs (Or  xs) = foldOr  fs (map (propFold fs) xs)
