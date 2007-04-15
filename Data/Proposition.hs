{-|
    This module imports and reexports all the necessary bits.
    
    The key module is "Data.Proposition.Internal", this contains
    the 'PropLit' and 'Prop' classes around which the entire library is
    based.
    
    Next there are three key 'Prop' instances:
    
    * 'PropSimple' provides a simple (and obviously correct)
    implementation of the class.
    
    * 'Formula' is an implementation based on an abstract syntax
    tree of a formula, similar to the Simple version, but which tries
    a lot harder to perform simplifications.
    
    * 'BDD' is an implementation based on Binary Decision Diagrams
    (BDD's). These have a fast and accurate Eq test.
    
    Then there are two 'Prop' instances which combine the previous ones:
    
    * 'PropAll' carries out all operations on all the three main propositions
    at once. If one disagrees then an error is given - useful mainly for testing.
    
    * 'PropFix' operates mainly on 'Formula', switching to 'BDD'
    for equality testing. This is intended for finding fixed points
    faster.
    
    There is one module in this library which is not including by this one.
    "Data.Proposition.Char" provides a 'PropLit' instance for 'Char'.

    We now present a short example, the code can be found in @Sample.hs@ in the root
    of the darcs repo.
    
    First step, lets import the necessary stuff:
    
    > import Data.Predicate        -- standard import
    > import Data.Predicate.Char   -- so we can place characters in a proposition
    
    Let us define a formula:
    
    > prop :: Prop p => p Char
    > prop = propAnds [propLit 'a' `propOr` propNot (propLit 'b')
    >                 ,propLit 'c' `propOr` propLit 'c'
    >                 ,propLit 'd' `propOr` propTrue]

    This example makes use of all the basic proposition operations. 'propLit' constructs
    a proposition from a literal. 'propNot' is negation of a proposition. 'propAnd' and
    'propAnds' are the equivalents to @&&@ and @and@, ditto with 'propOr' and 'propOrs'.
    
    We can now display this proposition as any of the standard propositions:
    
    > Main> prop :: PropSimple Char
    > (('a' v ~('b')) ^ ('c' v 'c'))

    This is the basic proposition, which has done @or True@ short-circuiting.
    
    > Main> prop :: Formula Char
    > ('c' ^ ('a' v ~'b'))
    
    This proposition has done @c || c = c@ transformation as well. It can also do
    more fancy simplifications as described by the 'PropLit' transformation.
    
    > Main> prop :: BDD Char
    > 'a' <'b' <'c' | False> | 'c'>
    
    This final proposition is a BDD. You can read this as a tree, where left is a
    False choice, and right is a True choice:
    
    >                a
    >               / \
    >              b   c
    >             / \
    >            c  False

    Now following the varibles will give you the logical result. Also note that the
    variables are ordered so @\'a\'@ will always be at the root.

    We can now perform various operations on the proposition, a couple of interesting
    ones are:
    
    We can now define an @upperCase@ operation:
    
    > upperCase x = propMap (propLit . toUpper) x
    >
    > Main> upperCase prop :: Formula Char
    > ('C' ^ ('A' v ~'B'))

    We can also change from one type of proposition to another:
    
    > Main> propRebuildSimple (prop  :: BDD Char)
    > ((~('a') v 'c') ^ ('a' v (~('b') ^ ('b' v 'c'))))

    And we can also test whether a proposition is True or False:
    
    > Main> propIsTrue (prop :: BDD Char)
    > False
    > Main> propIsFalse (prop :: BDD Char)
    > False

    There are lots of other operations of propositions, but these are the most common.
-}

module Data.Proposition(
	module Data.Proposition.Internal, module Data.Proposition,
	module Data.Proposition.BDD, module Data.Proposition.Formula,
    module Data.Proposition.Simple, module Data.Proposition.Fix,
    module Data.Proposition.All
	) where

import Data.Proposition.BDD
import Data.Proposition.Formula
import Data.Proposition.Simple
import Data.Proposition.Fix
import Data.Proposition.All
import Data.Proposition.Internal

