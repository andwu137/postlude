# postlude
This is a 'replacement' library for prelude. Inspired by the words "typeclass",
and tired of things like `Alternative vs MonadPlus` and `String = [Char]`.
Its purpose is mostly educational, as teaching students the life of legacy code
is a bit of a pain. Also, the ability to restructure the base library
to help students is a massive plus.

# Creation Classes of type (f a <-> a):
- Pure <-> Default
- Error | Unit <-> Empty

# Inheritance Chart
```
Functor -> Apply |
            Pure -> Monad
            Unit -> Alternative
```

# TODO:
- Add Inline / Inlineable PRAGMAs
- instances of Numerics for Int types
