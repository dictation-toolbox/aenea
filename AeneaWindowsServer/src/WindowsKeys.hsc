{-# LANGUAGE ForeignFunctionInterface
, CPP
, GeneralizedNewtypeDeriving
, OverloadedStrings #-}

module WindowsKeys (Key, keyPress, alt, ctrl, control, shift, k_0, a, tab) where

import System.Win32.Types
import Graphics.Win32.Key
import Foreign hiding (shift)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Text (Text)
import Control.Applicative

#include "Windows.h"
#include "winuser.h"
#include "winable.h"

data Key = Key { keyCode :: VKey
               , keyName :: Text
               , keyCharacter :: Maybe Char
               , keyRequiresShift :: Bool
               , keyIsModifier :: Bool }

alt = Key (#const VK_MENU) "alt" Nothing False True
ctrl = Key (#const VK_CONTROL) "ctrl" Nothing False True
control = Key (#const VK_CONTROL) "control" Nothing False True
shift = Key (#const VK_SHIFT) "shift" Nothing False True
k_0 = Key 0x30 "0" (Just '0') False False
a = Key 0x41 "a" (Just 'a') False False
tab = Key (#const VK_TAB) "tab" Nothing False False

keyPress :: Key -> IO ()
keyPress k = keyDown k >> keyUp k

keyUp :: Key -> IO ()
keyUp k = key k True

keyDown :: Key -> IO ()
keyDown k = key k False

key :: Key -> Bool -> IO ()
key k isDown = let code = fromIntegral $ keyCode k
                   direction = if isDown then 2 else 0
               in c_keybd_event code 0 direction 0

-- key :: DWORD -> Bool -> IO ()
-- key code = let c = fromIntegral code
--            in c_keybd_event c 0 2 0 >>
--               c_keybd_event c 0 0 0

key2 :: Int -> IO ()
key2 code = key2Internal code True >> key2Internal code False >> return ()

key2Internal :: Int -> Bool -> IO Int
key2Internal code isDown = let c = fromIntegral code
                               direction = if isDown then 0 else 2
            in fromIntegral <$> (withArrayLen [Input (#const INPUT_KEYBOARD) (Key' (KeybdInput c 0 direction 0 nullPtr))] $ \len array ->
               c_SendInput (fromIntegral len) array (fromIntegral (sizeOf (undefined :: Input))))

foreign import stdcall unsafe "winuser.h keybd_event"
        c_keybd_event :: BYTE
                      -> BYTE
                      -> DWORD
                      -> DWORD
                      -> IO ()
                 
data Input = Input { input_type :: DWORD
                   , input_union :: InputUnion}

instance Storable Input where
    sizeOf _ = #{size INPUT}
    alignment _ = 5

data InputUnion = Mouse MouseInput
                | Key' KeybdInput
                | Hardware HardwareInput

instance Storable InputUnion where
    sizeOf (Key' k) = sizeOf k
    sizeOf (Mouse m) = sizeOf m
    sizeOf (Hardware h) = sizeOf h
    alignment _ = maximum [ alignment (undefined :: MouseInput)
                          , alignment (undefined :: KeybdInput)
                          , alignment (undefined :: HardwareInput)]

data KeybdInput = KeybdInput { key_wVk :: WORD
                             , key_wScan :: WORD
                             , key_dwFlags :: DWORD
                             , key_time :: DWORD
                             , key_dwExtraInfo :: Ptr LONG}

instance Storable KeybdInput where
    sizeOf _ = #{size KEYBDINPUT}
    alignment _ = maximum [ alignment (undefined :: WORD)
                          , alignment (undefined :: DWORD)
                          , alignment (undefined :: Ptr LONG)]
    poke p kbInput = do
      #{poke KEYBDINPUT, wVk} p $ key_wVk kbInput
      #{poke KEYBDINPUT, wScan} p $ key_wScan kbInput
      #{poke KEYBDINPUT, dwFlags} p $ key_dwFlags kbInput
      #{poke KEYBDINPUT, time} p $ key_time kbInput
      #{poke KEYBDINPUT, dwExtraInfo} p $ key_dwExtraInfo kbInput
    peek p = do
      wVk <- (#peek KEYBDINPUT, wVk) p
      wScan <- (#peek KEYBDINPUT, wScan) p
      dwFlags <- (#peek KEYBDINPUT, dwFlags) p
      time <- (#peek KEYBDINPUT, time) p
      dwExtraInfo <- (#peek KEYBDINPUT, dwExtraInfo) p
      return $ KeybdInput wVk wScan dwFlags time dwExtraInfo

data MouseInput = MouseInput { mouse_dx :: LONG
                             , mouse_dy :: LONG
                             , mouse_mouseData :: DWORD
                             , mouse_dwFlags :: DWORD
                             , mouse_time :: DWORD
                             , mouse_dwExtraInfo :: Ptr LONG}

instance Storable MouseInput where
    sizeOf _ = #{size MOUSEINPUT}
    alignment _ = maximum [alignment (undefined :: WORD)
                          , alignment (undefined :: DWORD)
                          , alignment (undefined :: Ptr LONG)]

data HardwareInput = HardwareInput { hardware_uMsg :: DWORD
                                   , hardware_wParamL :: WORD
                                   , hardware_wParamH :: WORD}

instance Storable HardwareInput where
    sizeOf _ = #{size HARDWAREINPUT}
    alignment _ = max (alignment (undefined :: WORD)) (alignment (undefined :: DWORD))


foreign import stdcall unsafe "winuser.h SendInput"
        c_SendInput :: UINT
                    -> Ptr Input
                    -> CInt
                    -> IO UINT

