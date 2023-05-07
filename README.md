# churches
Church-encoded types in Haskell. Not useful in practice and **very** slow, but it is a fun exercise. https://en.wikipedia.org/wiki/Church_encoding

Modules:
- ChurchNatural (universally quantified using RankNTypes)
- ChurchBoolean (universally quantified using RankNTypes)
- ChurchTuple (typed tuple)
- ChurchList (typed list)
- ChurchFixed (fixed-point numbers with type-level precision)

using GHC extensions: DataKinds, FlexibleInstances, InstanceSigs, KindSignatures, RankNTypes, ScopedTypeVariables, TypeApplications