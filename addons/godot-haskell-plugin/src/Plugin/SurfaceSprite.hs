{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Plugin.SurfaceSprite
  ( GodotSurfaceSprite(..)
  , newGodotSurfaceSprite
  , setSurfaceTexture
  , getSurfaceTexture
  , updateSurfaceSprite
  , spriteShouldMove, setSpriteShouldMove
  , getSprite
  , InputEventType(..)
  , processClickEvent
  ) where

import Debug.C

import Control.Monad
import Data.Coerce

import           Linear
import           Plugin.Imports

import           Godot.Extra.Register
import           Godot.Core.GodotGlobalConstants
import qualified Godot.Core.GodotRigidBody   as RigidBody
import           Godot.Gdnative.Internal.Api
import qualified Godot.Methods               as G

import Plugin.SurfaceTexture

import Foreign

data GodotSurfaceSprite = GodotSurfaceSprite
  { _gwssObj     :: GodotObject
  , _gwssShouldMove :: TVar Bool
  , _gwssSprite :: TVar GodotSprite3D
  , _gwssShape :: TVar GodotBoxShape
  , _gwssTexture :: TVar GodotSurfaceTexture
  , _gwssSeat :: TVar (Ptr C'WlrSeat)
  }

instance Eq GodotSurfaceSprite where
  (==) = (==) `on` _gwssObj

instance GodotClass GodotSurfaceSprite where
  godotClassName = "SurfaceSprite"

instance ClassExport GodotSurfaceSprite where
  classInit obj =
    GodotSurfaceSprite obj
                  <$> atomically (newTVar True)
                  <*> atomically (newTVar (error "didn't init sprite")) <*> atomically (newTVar (error "didn't init shape"))
                  <*> atomically (newTVar (error "didn't init texture")) <*> atomically (newTVar (error "didn't init seat"))
  classExtends = "RigidBody"
  classMethods =
    [ GodotMethod NoRPC "_input_event" inputEvent
    , GodotMethod NoRPC "_ready" ready
    ]

instance HasBaseClass GodotSurfaceSprite where
  type BaseClass GodotSurfaceSprite = GodotRigidBody
  super (GodotSurfaceSprite obj _ _ _ _ _ ) = GodotRigidBody obj

newGodotSurfaceSprite :: GodotSurfaceTexture -> (Ptr C'WlrSeat) -> IO GodotSurfaceSprite
newGodotSurfaceSprite tex seat = do
  gwss <- "res://addons/godot-haskell-plugin/SurfaceSprite.gdns"
    & unsafeNewNS id "Object" []
    >>= godot_nativescript_get_userdata
    >>= deRefStablePtr . castPtrToStablePtr

  sprite <- unsafeInstance GodotSprite3D "Sprite3D"
  G.set_pixel_size sprite 0.001
  G.add_child gwss (safeCast sprite) True
  G.set_flip_h sprite True

  shape <- unsafeInstance GodotBoxShape "BoxShape"
  ownerId <- G.create_shape_owner gwss (safeCast gwss)
  G.shape_owner_add_shape gwss ownerId (safeCast shape)

  atomically $ writeTVar (_gwssSprite gwss) sprite
  atomically $ writeTVar (_gwssShape gwss) shape
  atomically $ writeTVar (_gwssTexture gwss) tex
  atomically $ writeTVar (_gwssSeat gwss) seat
  return gwss

setSurfaceTexture :: GodotSurfaceSprite -> GodotSurfaceTexture -> IO ()
setSurfaceTexture gwss tex = do
  atomically $ writeTVar (_gwssTexture gwss) tex
  sprite <- atomically $ readTVar (_gwssSprite gwss)
  G.set_texture sprite (safeCast tex)
  sizeChanged gwss

getSurfaceTexture :: GodotSurfaceSprite -> IO GodotSurfaceTexture
getSurfaceTexture gwss = atomically $ readTVar (_gwssTexture gwss)

updateSurfaceSprite :: GodotSurfaceSprite -> IO ()
updateSurfaceSprite gwss = do
  sprite <- atomically $ readTVar (_gwssSprite gwss)
  tex <- atomically $ readTVar (_gwssTexture gwss)
  updateSurfaceTexture tex -- not implemented yet
  G.set_texture sprite (safeCast tex)
  sizeChanged gwss

sizeChanged :: GodotSurfaceSprite -> IO ()
sizeChanged gwss = do
  sprite <- atomically $ readTVar (_gwssSprite gwss)
  aabb <- G.get_aabb sprite
  size <- godot_aabb_get_size aabb
  shape <- atomically $ readTVar (_gwssShape gwss)

  size' <- godot_vector3_operator_divide_scalar size 2

  G.set_extents shape size'

getSprite :: GodotSurfaceSprite -> IO GodotSprite3D
getSprite gwss = atomically $ readTVar (_gwssSprite gwss)

spriteShouldMove :: GodotSurfaceSprite -> IO Bool
spriteShouldMove gwss = do
  en <- atomically $ readTVar (_gwssShouldMove gwss)
  if en then do
    sprite <- atomically $ readTVar (_gwssSprite gwss)
    aabb <- G.get_aabb sprite
    size <- godot_aabb_get_size aabb
    vsize <- fromLowLevel size
    return (vsize > 0)
    else return False


setSpriteShouldMove :: GodotSurfaceSprite -> Bool -> IO ()
setSpriteShouldMove gwss = atomically . writeTVar (_gwssShouldMove gwss)

ready :: GFunc GodotSurfaceSprite
ready self _ = do
  G.set_mode self RigidBody.MODE_KINEMATIC
  toLowLevel VariantNil

inputEvent :: GFunc GodotSurfaceSprite
inputEvent self args = do
  case toList args of
    [_cam, evObj, clickPosObj, _clickNormal, _shapeIdx] ->  do
      ev <- fromGodotVariant evObj
      clickPos <- fromGodotVariant clickPosObj
      processInputEvent self ev clickPos
      godot_object_destroy ev
    _ -> putStrLn "expected 5 arguments in _input_event"
  toLowLevel VariantNil

data InputEventType
  = Motion
  | Button Bool Int

processInputEvent :: GodotSurfaceSprite -> GodotObject -> GodotVector3 -> IO ()
processInputEvent gwss ev clickPos = do
  whenM (ev `isClass` "InputEventMouseMotion") $ processClickEvent gwss Motion clickPos
  whenM (ev `isClass` "InputEventMouseButton") $ do
    let ev' = GodotInputEventMouseButton (coerce ev)
    pressed <- G.is_pressed ev'
    button <- G.get_button_index ev'
    processClickEvent gwss (Button pressed button) clickPos

processClickEvent :: GodotSurfaceSprite -> InputEventType -> GodotVector3 -> IO ()
processClickEvent gwss evt clickPos = do putStrLn "processClickEvent not yet implemented."
  {-
  lpos <- G.to_local gwss clickPos >>= fromLowLevel
  sprite <- atomically $ readTVar (_gwssSprite gwss)
  aabb <- G.get_aabb sprite
  size <- godot_aabb_get_size aabb >>= fromLowLevel

  let topleftPos = V2 (size ^. _x / 2 - lpos ^. _x ) (size ^. _y / 2 - lpos ^. _y)
  let scaledPos = liftI2 (/) topleftPos (size ^. _xy)

  rect <- G.get_item_rect sprite
  recSize <- godot_rect2_get_size rect >>= fromLowLevel

  let coords = liftI2 (*) recSize scaledPos

  -- coords = surface coordinates in pixel with (0,0) at top left
  let sx = truncate (256 * coords ^. _x)
      sy = truncate (256 * coords ^. _y)
  case evt of
    Motion -> processMouseMotionEvent sx sy
    Button pressed button ->  processMouseButtonEvent sx sy pressed button

  where
    getMsec = do
      time <- getTime Realtime
      let msec = fromIntegral $ toNanoSecs time `div` 1000000
      return msec
    processMouseMotionEvent sx sy =  do
      msec <- getMsec
      seat <- atomically $ readTVar (_gwssSeat gwss)
      pointer <- weston_seat_get_pointer seat

      pointer_send_motion pointer msec sx sy

    processMouseButtonEvent sx sy pressed button = do

      msec <- getMsec
      gwst <- atomically $ readTVar $ _gwssTexture gwss
      view <- atomically $ readTVar $ _gwstView gwst

      seat <- atomically $ readTVar (_gwssSeat gwss)
      kbd <- weston_seat_get_keyboard seat
      pointer <- weston_seat_get_pointer seat

      when pressed $ weston_pointer_set_focus pointer view sx sy

      ws <- atomically $ readTVar $ _gwstSurface gwst
      weston_keyboard_set_focus kbd ws
      weston_pointer_send_button pointer msec (toWestonButton button) (fromIntegral $ fromEnum pressed) --see libinput and wayland for enums; converting later

    toWestonButton BUTTON_LEFT = 0x110
    toWestonButton BUTTON_RIGHT = 0x111
    toWestonButton BUTTON_MIDDLE = 0x112
    toWestonButton BUTTON_WHEEL_UP = 0x151
    toWestonButton BUTTON_WHEEL_DOWN = 0x150
    toWestonButton _ = 0x110
   -}