# postlude

# Creation Classes of type (f a <-> a):
Pure <-> Default
Error | Unit <-> Empty

# Inheritance Chart
Functor -> Apply |
            Pure -> Applicative
                 -> Monad
            Unit -> Alternative

Semigroup |
    Empty -> Monoid

# TODO:
- Add Inline / Inlineable PRAGMAs
