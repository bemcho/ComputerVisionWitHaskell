-- Credits to https://gist.github.com/danbst/470f7e23a14cab4e6e3e
module UI.OpenGLUtils
  ( drawTexture
  , bindBMPTexture
  , loadTex
  ) where

import qualified Codec.BMP                 as BMP
import           Control.Monad
import           Data.ByteString           (ByteString (..))
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Unsafe    as BSU
import           Data.IORef
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Graphics.GLUtil           (readTexture, texture2DWrap)
import           Graphics.Rendering.OpenGL (($=))
import           Graphics.Rendering.OpenGL as GL
import           Unsafe.Coerce

oSCREEN_WIDTH = 640 :: Int

oSCREEN_HEIGHT = 480 :: Int

bindBMPTexture texId filePath = do
  Right image <- BMP.readBMP filePath
  let dta = BMP.bmpRawImageData image
  bPtr <- BSU.unsafeUseAsCString dta $ \cstr -> return (castPtr cstr)
  GL.textureBinding GL.Texture2D $= Just texId
  let size = getSize image
      glSize = GL.TextureSize2D (gsizei $ fst size) (gsizei $ snd size)
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8 glSize 0 (GL.PixelData GL.BGR GL.UnsignedByte bPtr)
  return size
  where
    getSize bmp =
      let (BMP.InfoV3 info) = BMP.bmpBitmapInfo bmp
          h = BMP.dib3Height info
          w = BMP.dib3Width info
       in (fromIntegral w, fromIntegral h)

loadTex :: FilePath -> IO TextureObject
loadTex f = do
  t <- either error id <$> readTexture f
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
--  texture2DWrap $= (Repeated, ClampToEdge)
  return t

drawTexture tex (tSizeX, tSizeY) (posX, posY) (sizeX, sizeY) = do
  clear [GL.ColorBuffer]
--  setDefaultTextureSettings
  withTexture tex $ do
    let (outHalfWidth, outHalfHeight) =
          ((fromIntegral sizeX) / (fromIntegral oSCREEN_WIDTH), (fromIntegral sizeY) / (fromIntegral oSCREEN_HEIGHT)) :: ( Float
                                                                                                                         , Float)
        (inWidth, inHeight) =
          ((fromIntegral sizeX) / (fromIntegral tSizeX), (fromIntegral sizeY) / (fromIntegral tSizeY))
        (inPosX, inPosY) = ((fromIntegral posX) / (fromIntegral tSizeX), (fromIntegral posY) / (fromIntegral tSizeY))
    GL.renderPrimitive GL.Polygon $
      zipWithM_
        makePoint
        [ (0.0 - outHalfWidth, 0.0 - outHalfHeight)
        , (outHalfWidth, 0 - outHalfHeight)
        , (outHalfWidth, outHalfHeight)
        , (0.0 - outHalfWidth, outHalfHeight)
        ]
        ([ (inPosX, 1 - (inPosY + inHeight))
         , (inPosX + inWidth, 1 - (inPosY + inHeight))
         , (inPosX + inWidth, 1 - inPosY)
         , (inPosX, 1 - inPosY)
         ] :: [(Float, Float)])
  where
    makePoint (px, py) (tx, ty) = do
      texCoord2 tx ty
      vertex2 px py
--
--setDefaultTextureSettings = do
--  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
--  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
--  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')

withTexture tex f = do
  GL.texture GL.Texture2D $= GL.Enabled
  GL.textureBinding GL.Texture2D $= Just tex
  f
  GL.texture GL.Texture2D $= GL.Disabled

texCoord2 x y = GL.texCoord $ GL.TexCoord2 (gf x) (gf y)

vertex2 x y = GL.vertex $ GL.Vertex2 (gf x) (gf y)

gf :: Float -> GL.GLfloat
gf = realToFrac

gsizei :: Int -> GL.GLsizei
{-# INLINE gsizei #-}
gsizei x = unsafeCoerce x
