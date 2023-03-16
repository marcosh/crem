{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The `RenderableVertices` class describes which values of type @a@ should
-- be rendered when drawing a graph (or a topology) with vertices of type @a@
module Crem.Render.RenderableVertices where

import "base" Data.Functor.Const (Const (..))
import "base" Data.Functor.Identity (Identity (..))
import "base" Data.Monoid (Dual (..), Product (..), Sum (..))
import "base" Data.Ord (Down (..))
import "base" Data.Proxy (Proxy (..))
import "base" Data.Semigroup (All (..), Any (..), First (..), Last (..), Max (..), Min (..))
import "base" Data.Void (Void)

-- | The `RenderableVertices` class is implemented just as a list of elements
-- of type @a@.
class RenderableVertices a where
  vertices :: [a]

-- | This is a newtype to be used with `deriving via`. If `a` has instances for
-- `Enum` and `Bounded`, then `AllVertices a` has an instance of
-- `RenderableVertices` which lists all the terms of type `a`.
newtype AllVertices a = AllVertices a

instance (Enum a, Bounded a) => RenderableVertices (AllVertices a) where
  vertices :: [AllVertices a]
  vertices = AllVertices <$> [minBound .. maxBound]

instance RenderableVertices Void where
  vertices :: [Void]
  vertices = []

deriving via AllVertices () instance RenderableVertices ()

deriving via AllVertices Bool instance RenderableVertices Bool

deriving newtype instance RenderableVertices All

deriving newtype instance RenderableVertices Any

deriving via AllVertices Ordering instance RenderableVertices Ordering

instance RenderableVertices a => RenderableVertices (Maybe a) where
  vertices :: [Maybe a]
  vertices =
    Nothing : (Just <$> vertices)

deriving newtype instance RenderableVertices a => RenderableVertices (Min a)

deriving newtype instance RenderableVertices a => RenderableVertices (Max a)

deriving newtype instance RenderableVertices a => RenderableVertices (First a)

deriving newtype instance RenderableVertices a => RenderableVertices (Last a)

deriving newtype instance RenderableVertices a => RenderableVertices (Identity a)

deriving newtype instance RenderableVertices a => RenderableVertices (Dual a)

deriving newtype instance RenderableVertices a => RenderableVertices (Sum a)

deriving newtype instance RenderableVertices a => RenderableVertices (Down a)

deriving newtype instance RenderableVertices a => RenderableVertices (Product a)

instance RenderableVertices (Proxy a) where
  vertices :: [Proxy a]
  vertices = [Proxy]

deriving newtype instance RenderableVertices a => RenderableVertices (Const a b)

instance (RenderableVertices a, RenderableVertices b) => RenderableVertices (Either a b) where
  vertices :: [Either a b]
  vertices =
    (Left <$> vertices)
      <> (Right <$> vertices)

instance (RenderableVertices a, RenderableVertices b) => RenderableVertices (a, b) where
  vertices :: [(a, b)]
  vertices = [(a, b) | a <- vertices, b <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c) => RenderableVertices (a, b, c) where
  vertices :: [(a, b, c)]
  vertices = [(a, b, c) | a <- vertices, b <- vertices, c <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c, RenderableVertices d) => RenderableVertices (a, b, c, d) where
  vertices :: [(a, b, c, d)]
  vertices = [(a, b, c, d) | a <- vertices, b <- vertices, c <- vertices, d <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c, RenderableVertices d, RenderableVertices e) => RenderableVertices (a, b, c, d, e) where
  vertices :: [(a, b, c, d, e)]
  vertices = [(a, b, c, d, e) | a <- vertices, b <- vertices, c <- vertices, d <- vertices, e <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c, RenderableVertices d, RenderableVertices e, RenderableVertices f) => RenderableVertices (a, b, c, d, e, f) where
  vertices :: [(a, b, c, d, e, f)]
  vertices = [(a, b, c, d, e, f) | a <- vertices, b <- vertices, c <- vertices, d <- vertices, e <- vertices, f <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c, RenderableVertices d, RenderableVertices e, RenderableVertices f, RenderableVertices g) => RenderableVertices (a, b, c, d, e, f, g) where
  vertices :: [(a, b, c, d, e, f, g)]
  vertices = [(a, b, c, d, e, f, g) | a <- vertices, b <- vertices, c <- vertices, d <- vertices, e <- vertices, f <- vertices, g <- vertices]

instance (RenderableVertices a, RenderableVertices b, RenderableVertices c, RenderableVertices d, RenderableVertices e, RenderableVertices f, RenderableVertices g, RenderableVertices h) => RenderableVertices (a, b, c, d, e, f, g, h) where
  vertices :: [(a, b, c, d, e, f, g, h)]
  vertices = [(a, b, c, d, e, f, g, h) | a <- vertices, b <- vertices, c <- vertices, d <- vertices, e <- vertices, f <- vertices, g <- vertices, h <- vertices]
