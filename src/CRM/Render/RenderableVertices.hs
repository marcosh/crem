{-# LANGUAGE UndecidableInstances #-}

module CRM.Render.RenderableVertices where

class RenderableVertices a where
  vertices :: [a]

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
