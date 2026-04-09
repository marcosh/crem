{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

#if __GLASGOW_HASKELL__ >= 908
{-# LANGUAGE TypeAbstractions #-}
#endif

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 908
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wmissing-poly-kind-signatures
{-# OPTIONS_GHC -Wno-missing-poly-kind-signatures #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag-Wmissing-role-annotations
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}
#endif

-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wredundant-constraints
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunticked-promoted-constructors
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/using-warnings.html#ghc-flag--Wunused-type-patterns
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

-- | A `Topology` is a list of allowed transition for a state machine.
-- We are using it to enforce that only allowed transitions could be performed.
module Crem.Topology
  ( Topology (..)
  , TopologySym0
  , STopology (..)
  , AllowTransition (..)
  , AllowedTransition (..)
  , trivialTopology
  , sTrivialTopology
  , TrivialTopology
  , allowAllTopology
  , sAllowAllTopology
  , AllowAllTopology
  )
where

import "nothunks" NoThunks.Class (NoThunks (..))
import "singletons-base" Data.Singletons.Base.TH
import "singletons-base" Prelude.Singletons

-- * Topology

-- | A `Topology` is a description of the topology of a state machine
-- It contains the collection of allowed transitions.
-- Since we are using this information at the type level, and then we want to
-- bring it down to the value level, we wrap it in `singletons`
$( singletons
     [d|
       newtype Topology vertex = Topology
         {edges :: [(vertex, [vertex])]}
       |]
 )

-- ** AllowedTransition

-- | An value of type of @AllowedTransition topology initial final@ is a proof
-- that the @topology@ allows transitions from the @initial@ to the @final@
-- state
data AllowTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex) where
  -- | We always allow an edge from a vertex to itself
  AllowIdentityEdge :: AllowTransition topology a a
  -- | If @a@ is the start and @b@ is the end of the first edge,
  -- then @map@ contains an edge from @a@ to @b@
  AllowFirstEdge :: AllowTransition ('Topology ('(a, b ': l1) ': l2)) a b
  -- | If we know that we have an edge from @a@ to @b@ in a topology,
  -- then we also have an edge from @a@ to @b@ if we add another edge out of @a@
  AllowAddingEdge
    :: AllowTransition ('Topology ('(a, l1) ': l2)) a b
    -> AllowTransition ('Topology ('(a, x ': l1) ': l2)) a b
  -- | If we know that we have an edge from @a@ to @b@ in @map@,
  -- then we also have an edge from @a@ to @b@ if we add another vertex
  AllowAddingVertex
    :: AllowTransition ('Topology tmap) a b
    -> AllowTransition ('Topology (x ': tmap)) a b

instance NoThunks (AllowTransition topology initial final) where
  showTypeOf _ = "AllowTransition"
  wNoThunks ctxt at =
    case at of
      AllowIdentityEdge -> return Nothing
      AllowFirstEdge -> return Nothing
      AllowAddingEdge x -> noThunks ctxt x
      AllowAddingVertex x -> noThunks ctxt x

-- | The `AllowedTransition` type class enables to automatically perform proof search
-- for a `AllowTransition` term.
-- It has an instance for every constructor of `AllowTransition`
class AllowedTransition (topology :: Topology vertex) (initial :: vertex) (final :: vertex) where
  allowsTransition :: AllowTransition topology initial final

instance {-# INCOHERENT #-} AllowedTransition ('Topology ('(a, b ': l1) ': l2)) a b where
  allowsTransition = AllowFirstEdge

instance {-# INCOHERENT #-} (AllowedTransition ('Topology ('(a, l1) ': l2)) a b) => AllowedTransition ('Topology ('(a, x ': l1) ': l2)) a b where
  allowsTransition =
    AllowAddingEdge (allowsTransition :: AllowTransition ('Topology ('(a, l1) ': l2)) a b)

instance {-# INCOHERENT #-} (AllowedTransition ('Topology tmap) a b) => AllowedTransition ('Topology (x ': tmap)) a b where
  allowsTransition =
    AllowAddingVertex (allowsTransition :: AllowTransition ('Topology tmap) a b)

instance {-# INCOHERENT #-} AllowedTransition topology a a where
  allowsTransition = AllowIdentityEdge

-- ** Trivial topology

-- | The trivial topology only allows identity transitions.
-- Given a type @a@ for vertices, only trivial transitions, i.e. staying
-- at the same vertex, are allowed
$( singletons
     [d|
       trivialTopology :: Topology a
       trivialTopology = Topology []
       |]
 )

-- ** Allow all topology

-- | Given a type @a@ for vertices, every transition from one vertex to
-- any other is allowed
$( singletons
     [d|
       allowAllTopology :: (Bounded a, Enum a) => Topology a
       allowAllTopology = Topology [(a, [minBound .. maxBound]) | a <- [minBound .. maxBound]]
       |]
 )
