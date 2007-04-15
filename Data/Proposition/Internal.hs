{-|
    The module containing 'Prop' and 'PropLit'.
-}

module Data.Proposition.Internal where

import Control.Monad.Identity
import Control.Monad.State


-- | How to fold a proposition, named to stop the
--   ordering being confusing.
data PropFold lit res = PropFold {foldOr  :: [res] -> res, foldAnd :: [res] -> res,
                                  foldNot :: res -> res  , foldLit :: lit -> res  }


-- | Standard proposition class, @p@ must be a higher kind
class Prop p where
    -- | Literal true
    propTrue  :: p a
    -- | Literal false
    propFalse :: p a
    -- | Returns True if the formula is equal to True
    propIsTrue  :: PropLit a => p a -> Bool
    -- | Returns True if the formula is equal to False
    propIsFalse :: PropLit a => p a -> Bool
    
    -- | Make a proposition out of a literal
    propLit :: PropLit a => a -> p a
    -- | @(&&)@ equivalent
    propAnd :: PropLit a => p a -> p a -> p a
    -- | @(||)@ equivalent
    propOr  :: PropLit a => p a -> p a -> p a
    -- | @not@ equivalent
    propNot :: PropLit a => p a -> p a

    -- | a monadic map over a proposition, replacing each item
    --   with a whole proposition
    propMapM :: (Monad m, PropLit a) => (a -> m (p a)) -> p a -> m (p a)
    
    -- | a complete fold over a proposition
    propFold :: PropLit a => PropFold a res -> p a -> res
    
    -- non essential methods
    
    -- | @and@ equivalent, a fold on 'propAnd'
    propAnds :: PropLit a => [p a] -> p a
    propAnds = foldr propAnd propTrue
    
    -- | @or@ equivalent, a fold on 'propOr'
    propOrs :: PropLit a => [p a] -> p a
    propOrs = foldr propOr propFalse

    -- | Convert a proposition from one type to another, typically
    --   use @propRebuildType@ instead - otherwise defaulting is an issue
    propRebuild :: (PropLit a, Prop q) => p a -> q a
    propRebuild = propFold (PropFold propOrs propAnds propNot propLit)

    -- | Non monadic map, can be defined in terms of 'propMapM'
    propMap  :: PropLit a => (a -> p a) -> p a -> p a
    propMap f = runIdentity . propMapM (return . f)

    -- | All literals out of the proposition, can be defined
    -- in terms of 'propMapM'
    propAll  :: PropLit a => p a -> [a]
    propAll x = execState (propMapM f x) []
        where f x = modify (x:) >> return (propLit x)

    -- | Simplify a proposition, defaults to @id@        
    propSimplify :: PropLit a => p a -> p a
    propSimplify = id
    
    -- | Convert a boolean to a proposition, not essential
    propBool :: Bool -> p a
    propBool b = if b then propTrue else propFalse
    
    -- | Change from on one literal type to another
    propChange :: (PropLit a, PropLit b) => (a -> p b) -> p a -> p b
    propChange f = propFold (PropFold propOrs propAnds propNot f)


-- | How to reduce a pair of values
data Reduce a = Value a      -- ^ They reduce to one single value
              | Literal Bool -- ^ They reduce to a literal boolean
              | None         -- ^ They do not reduce
              deriving (Show,Eq)

-- based on the instance for Maybe
instance Monad Reduce where
    Value x   >>= k = k x
    None      >>= k = None
    Literal x >>= k = Literal x
    return        = Value
    fail s        = None


-- | An instance for things that go inside a 'Prop'.
--   The simplification methods are designed to be terminating.
--   All have default instances.
class (Show a, Ord a) => PropLit a where
    -- | Reduce two variables combined with @&&@
    (?/\) :: a -> a -> Reduce a
    -- | Reduce two variables combined with @||@
    (?\/) :: a -> a -> Reduce a
    -- | Given a list of variables set to True\/False, when given
    --   an extra value, does it reduce to True or False
    (?=>) :: [(a,Bool)] -> a -> Maybe Bool
    -- | Simplify a single literal on its own
    simp :: a -> Maybe Bool
    -- | Negate a literal
    litNot :: a -> Maybe a
    
    a ?/\ b = None
    a ?\/ b = None
    a ?=> b = lookup b a
    simp a = Nothing
    litNot a = Nothing




-- | Try and simplify an Or, using implies
reduceOrWithImp :: PropLit a => a -> a -> Maybe (Reduce a)
{-
a b | a => b | a v b | a v b == b
F F     T        F        T
F T     T        T        T
T F     F        T        ?
T T     T        T        T 
-}
reduceOrWithImp a b
    | [(a,True)] ?=> b == Just True = Just $ Value b
    | [(b,True)] ?=> a == Just True = Just $ Value a
    | otherwise = Nothing



-- | Try and simplify an And, using implies
reduceAndWithImp :: PropLit a => a -> a -> Maybe (Reduce a)
{-
a b | a => b | a ^ b | a ^ b == a
F F     T        F        T
F T     T        F        T
T F     F        F        ?
T T     T        T        T

a b | a => ¬b | a ^ b | a ^ b == F
F F      T        F        T
F T      T        F        T
T F      T        F        T
T T      F        T        ?
-}
reduceAndWithImp a b
    | a_implies_b == Just True  = Just $ Value a
    | a_implies_b == Just False = Just $ Literal False
    | b_implies_a == Just True  = Just $ Value b
    | b_implies_a == Just False = Just $ Literal False
    | otherwise = Nothing
    where
        a_implies_b = [(a,True)] ?=> b
        b_implies_a = [(b,True)] ?=> a
