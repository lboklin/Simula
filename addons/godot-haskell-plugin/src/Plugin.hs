{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Plugin (registerClasses) where

import           Godot.Extra.Register

import           Plugin.Simula
import           Plugin.SimulaController
import           Plugin.Compositor
import           Plugin.SurfaceSprite
import           Plugin.SurfaceTexture


registerClasses :: GdnativeHandle -> IO ()
registerClasses desc = do
  let reg constr = registerClass $ RegClass desc constr
  reg $ classInit @GodotSimula
  reg $ classInit @GodotSimulaController
  reg $ classInit @GodotCompositor
  reg $ classInit @GodotSurfaceSprite
  reg $ classInit @GodotSurfaceTexture
