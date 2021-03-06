{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | this data is for the vulkan engine
module Vulk.VulkData where
-- some auxillary data structs
import Graphics.Vulkan.Core_1_0

-- | data required to create a command
--   command buffer from drawState
data CmdBuffData = CmdBuffData
         { cbGQData     ∷ GQData
         , cbPipeline   ∷ VkPipeline
         , cbRenderPass ∷ VkRenderPass
         , cbPipeLayout ∷ VkPipelineLayout
         , cbSwapInfo   ∷ SwapchainInfo
         , cbFramebuffs ∷ [VkFramebuffer]
         , cbDescSets   ∷ VkDescriptorSet }

-- | TODO: see if this is already defined somewhere else
data SwapchainInfo = SwapchainInfo
         { swapchain     ∷ VkSwapchainKHR
         , swapImgs      ∷ [VkImage]
         , swapImgFormat ∷ VkFormat
         , swapExtent    ∷ VkExtent2D
         } deriving (Eq, Show)

-- | the data required to create the texture
--   from the graphicsqueue and command pool
data GQData = GQData
         { pdev    ∷ VkPhysicalDevice
         , dev     ∷ VkDevice
         , cmdPool ∷ VkCommandPool
         , gqueue  ∷ VkQueue }

