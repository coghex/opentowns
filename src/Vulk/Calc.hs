{-# LANGUAGE Strict #-}
{-# LANGUAGE KindSignatures #-}
-- | functions used by the load thread to convert a list of tiles into
--   a tuple of dataframes consisting of verticies and indicies ready to
--   go straight into a command buffer. requires much computational work
module Vulk.Calc
where
-- translations from lua state to draw
-- state are defined, should be run
-- outside of parent thread
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0 ( Word32 )
import Numeric.DataFrame
import Data ( Color(..) )
import Load.Data ( Tile(..) )
import Vulk.Atlas ( indexAtlas )
import Vulk.Vertex
    ( atLeastThree, Vertex(..) )

-- | determines dataframes from drawstate
calcVertices ∷ [Tile]
  → (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices ts = (vertices ts, indices ts)

-- | a simple base matrix to work off of, each square has four vertecies.
--   and  is drawn by vulkan as a set of two triangles.
-- TODO: great speedups could happen if we learn how to use this library
-- and build the dataframes directly from the tiles instead of making
-- a base identity value and altering it with each transformation
vertsqs ∷ [DataFrame Vertex ('[] ∷ [Nat])]
vertsqs
  = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1)
                 (vec3 0 1 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1)
                 (vec3 1 1 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1)
                 (vec3 1 0 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1)
                 (vec3 0 0 0.1) (vec3 0 0 0) ]
-- | combines all Tiles into a dataframe
vertices ∷ [Tile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices (1∷Int) ts
  where combineVertices _    [] = []
    -- GTiles are static and have little data, perfect for a background
    -- or a massive amount of text. since this engine rarely uses these
    -- features, there are few GTiles, but they are nice for debugging
        combineVertices nDyn
          ((GTile (x',y') (xscale',yscale') (ax',ay') (sx,sy) t' c'):tts)
            = withColor c' (withTC (indexAtlas ax ay sx sy)
              (withTC (+ vec3 0 0 t) (withPos (+ vec4 x0 y0 0 0)
              (withScale (* vec3 xscale yscale 1) vertsqs))))
                ⧺ combineVertices nDyn tts where
          -- note that all tile positions are multiplied by 2
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          withPos f = map (\(S v) → S v { pos
            = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withColor (Color r g b a)
            = map (\(S v) → S v { color = vec4 r' g' b' a' }) where
                 r' = fromIntegral r / 255
                 g' = fromIntegral g / 255
                 b' = fromIntegral b / 255
                 a' = fromIntegral a / 255
  -- DTiles contain extra functionality such as position, and atlas index
  -- when tiles are processed into dynamic data each DTile is matched with
  -- a buffer and the trans functions only apply dyndata to their
  -- corresponding buffer, allowing for different size DTiles
  -- TODO: add color
        combineVertices nDyn
          ((DTile _ (x',y') (xscale',yscale')
          (ax',ay') (sx,sy) moves t'):tts)
            = withColor (withMove (+ vec3 1 dyn moves')
              (withTC (indexAtlas ax ay sx sy) (withTC (+ vec3 0 0 t)
              (withPos (+ vec4 x0 y0 0 0)
              (withScale (* vec3 xscale yscale 1) vertsqs)))))
                ⧺ combineVertices (nDyn + 1) tts where
          (x0,y0) = (realToFrac(2*x'), realToFrac(2*y'))
          ( ax,  ay) = (fromIntegral ax', fromIntegral ay')
          (xscale, yscale)  = (realToFrac xscale', realToFrac yscale')
          t = fromIntegral t'
          dyn = fromIntegral nDyn
          moves' = if moves then 1 else 0
          withPos f = map (\(S v) → S v { pos
            = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })
          withColor = map (\(S v) → S v { color = vec4 1 1 1 1 })

-- | vulkan will draw the dataframe for each tile in this order, six
--   points for the 6 verticies in two triangles laid together squarely
--   this function is suprisingly slow
indices ∷ [Tile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ combineIndices tiles
combineIndices ∷ ∀ a. (Num a) ⇒ [Tile] → [a]
combineIndices []           = []
combineIndices (_:tiles) = oneRectIndices ⧺ map (+4) (combineIndices tiles)
  where oneRectIndices = [0,3,2,2,1,0]

