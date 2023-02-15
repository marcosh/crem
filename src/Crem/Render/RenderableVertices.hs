{-# LANGUAGE UndecidableInstances #-}

-- | The `RenderableVertices` class describes which values of type @a@ should
-- be rendered when drawing a graph (or a topology) with vertices of type @a@
module Crem.Render.RenderableVertices where

-- | The `RenderableVertices` class is implemented just as a list of elements
-- of type @a@.
class RenderableVertices a where
  vertices :: [a]

-- | If @a@ has `Enum` and `Bounded` instances, we have a way to enumerate all
-- the terms of type @a@.
--
-- Be careful to use this instance for types which are actually too big, like
-- `Int`. You probably don't want to print out every possible integer.
instance {-# OVERLAPPABLE #-} (Enum a, Bounded a) => RenderableVertices a where
  vertices :: [a]
  vertices = [minBound .. maxBound]

instance RenderableVertices a => RenderableVertices (Maybe a) where
  vertices :: [Maybe a]
  vertices =
    Nothing : (Just <$> vertices)

instance (RenderableVertices a, RenderableVertices b) => RenderableVertices (Either a b) where
  vertices :: [Either a b]
  vertices =
    (Left <$> vertices)
      <> (Right <$> vertices)

instance (RenderableVertices a, RenderableVertices b) => RenderableVertices (a, b) where
  vertices :: [(a, b)]
  vertices = [(a, b) | a <- vertices, b <- vertices]
