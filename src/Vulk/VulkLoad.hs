{-# LANGUAGE Strict #-}
-- | loads textures from listed files
--   TODO: this whole file is temporary, until
--         the lua interface is complete
module Vulk.VulkLoad where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify)
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Prog.Data ( Env(envFontM), State(stNDefTex) )
import Sign.Var ( atomically, modifyTVar' )
import Vulk.Data ( TextureData(TextureData) )
import Vulk.VulkData ( GQData(GQData) )
import Vulk.Desc ( createDescriptorSetLayout )
import Vulk.Texture
    ( createFontImageViews,
      createTextureImageView,
      createTextureImageViews,
      createTextureSampler,
      createTextureSamplers,
      findDepthFormat,
      loadNTexs,
      textureImageInfos )
import Vulk.Pipeline ( createPipelineLayout )

-- | loads all the textures into layouts of
--   the descriptor sets and pipeline. an
--   empty string will just load default textures
--   and filepaths added will be ammended to that
loadVulkanTextures ∷ GQData → [FilePath] → Prog ε σ (TextureData)
loadVulkanTextures (GQData pdev dev cmdPool cmdQueue) fps = do
  -- the engine reserves the first few
  -- textures for default usage.
  let tex0Path     = "dat/tex/alpha.png"
      tex1Path     = "dat/tex/texture.jpg"
      menuPath     = "data/graphics/mainmenubg.png"
      logoPath     = "data/graphics/townslogo.png"
      uiPath       = "data/graphics/ui.png"
      ui2Path      = "data/graphics/ui2.png"
      loadPath     = "data/graphics/loading.png"
      fontPath     = "dat/font/asdf.ttf"
      texBoxPath   = "dat/tex/box"
  -- tex zero is just 32x32 alpha
  (textureView0, mipLevels0)
    ← createTextureImageView pdev dev cmdPool cmdQueue tex0Path
  textureSampler0 ← createTextureSampler dev mipLevels0
  -- tex one is the background
  (textureView1, mipLevels1)
    ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  textureSampler1 ← createTextureSampler dev mipLevels1
  -- logo, menu, UI, default textures
  (textureViewL, mipLevelsL)
    ← createTextureImageView pdev dev cmdPool cmdQueue logoPath
  textureSamplerL ← createTextureSampler dev mipLevelsL
  (textureViewM, mipLevelsM)
    ← createTextureImageView pdev dev cmdPool cmdQueue menuPath
  textureSamplerM ← createTextureSampler dev mipLevelsM
  (textureViewU, mipLevelsU)
    ← createTextureImageView pdev dev cmdPool cmdQueue uiPath
  textureSamplerU ← createTextureSampler dev mipLevelsU
  (textureViewU2, mipLevelsU2)
    ← createTextureImageView pdev dev cmdPool cmdQueue ui2Path
  textureSamplerU2 ← createTextureSampler dev mipLevelsU2
  (textureViewLd, mipLevelsLd)
    ← createTextureImageView pdev dev cmdPool cmdQueue loadPath
  textureSamplerLd ← createTextureSampler dev mipLevelsLd
  -- box texs are for info boxs
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texBoxPath
  let (btexs, bsamps) = unzip boxTexs
  -- font texs are generated from ttf
  fontData ← createFontImageViews pdev dev cmdPool cmdQueue fontPath 16
  let (fontTexs, fontMetrics) = unzip fontData
      (ftexs, fmipLvls) = unzip fontTexs
  env ← ask
  liftIO $ atomically $ modifyTVar' (envFontM env) $ \_ → Just fontMetrics
  fontSamplers ← createTextureSamplers dev fmipLvls
  -- mod texs are textures included by the lua files
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  texSamplersMod ← createTextureSamplers dev $ snd . unzip $ modTexViews
  let defaultTexs = [textureView0, textureView1] ⧺ ftexs ⧺ btexs
                  ⧺ [textureViewM,textureViewL,textureViewU
                    ,textureViewU2,textureViewLd]
      texViews = defaultTexs ⧺ fst (unzip modTexViews)
      texSamps = [textureSampler0, textureSampler1]
               ⧺ fontSamplers ⧺ bsamps ⧺ [textureSamplerM
                 ,textureSamplerL,textureSamplerU,textureSamplerU2
                 ,textureSamplerLd] ⧺ texSamplersMod
  modify $ \s → s { stNDefTex = length defaultTexs }
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout
                nimages descriptorTextureInfo depthFormat
  return texdata
