{-# LANGUAGE ForeignFunctionInterface
, CPP
, GeneralizedNewtypeDeriving
, OverloadedStrings #-}

module WindowsKeys where

import System.Win32.Types
import Graphics.Win32.Key
import Foreign hiding (shift)
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Data.Text (Text)
import Control.Applicative
import Control.Exception

#include "Windows.h"
#include "winuser.h"
#include "winable.h"

data Key = Key { keyCode :: VKey
               , keyCharacter :: Maybe Char
               , keyRequiresShift :: Bool
               , keyIsModifier :: Bool }

key_ALT = Key vK_MENU Nothing False True
key_CONTROL = Key vK_CONTROL Nothing False True
key_SHIFT = Key vK_SHIFT Nothing False True
key_0 = Key 0x30 (Just '0') False False
key_1 = Key 0x31 (Just '1') False False
key_2 = Key 0x32 (Just '2') False False
key_3 = Key 0x33 (Just '3') False False
key_4 = Key 0x34 (Just '4') False False
key_5 = Key 0x35 (Just '5') False False
key_6 = Key 0x36 (Just '6') False False
key_7 = Key 0x37 (Just '7') False False
key_8 = Key 0x38 (Just '8') False False
key_9 = Key 0x39 (Just '9') False False
key_A = Key 0x41 (Just 'A') True False
key_B = Key 0x42 (Just 'B') True False
key_C = Key 0x43 (Just 'C') True False
key_D = Key 0x44 (Just 'D') True False
key_E = Key 0x45 (Just 'E') True False
key_F = Key 0x46 (Just 'F') True False
key_G = Key 0x47 (Just 'G') True False
key_H = Key 0x48 (Just 'H') True False
key_I = Key 0x49 (Just 'I') True False
key_J = Key 0x4A (Just 'J') True False
key_K = Key 0x4B (Just 'K') True False
key_L = Key 0x4C (Just 'L') True False
key_M = Key 0x4D (Just 'M') True False
key_N = Key 0x4E (Just 'N') True False
key_O = Key 0x4F (Just 'O') True False
key_P = Key 0x50 (Just 'P') True False
key_Q = Key 0x51 (Just 'Q') True False
key_R = Key 0x52 (Just 'R') True False
key_S = Key 0x53 (Just 'S') True False
key_T = Key 0x54 (Just 'T') True False
key_U = Key 0x55 (Just 'U') True False
key_V = Key 0x56 (Just 'V') True False
key_W = Key 0x57 (Just 'W') True False
key_X = Key 0x58 (Just 'X') True False
key_Y = Key 0x59 (Just 'Y') True False
key_Z = Key 0x5A (Just 'Z') True False
key_a = Key 0x41 (Just 'a') False False
key_b = Key 0x42 (Just 'b') False False
key_c = Key 0x43 (Just 'c') False False
key_d = Key 0x44 (Just 'd') False False
key_e = Key 0x45 (Just 'e') False False
key_f = Key 0x46 (Just 'f') False False
key_g = Key 0x47 (Just 'g') False False
key_h = Key 0x48 (Just 'h') False False
key_i = Key 0x49 (Just 'i') False False
key_j = Key 0x4A (Just 'j') False False
key_k = Key 0x4B (Just 'k') False False
key_l = Key 0x4C (Just 'l') False False
key_m = Key 0x4D (Just 'm') False False
key_n = Key 0x4E (Just 'n') False False
key_o = Key 0x4F (Just 'o') False False
key_p = Key 0x50 (Just 'p') False False
key_q = Key 0x51 (Just 'q') False False
key_r = Key 0x52 (Just 'r') False False
key_s = Key 0x53 (Just 's') False False
key_t = Key 0x54 (Just 't') False False
key_u = Key 0x55 (Just 'u') False False
key_v = Key 0x56 (Just 'v') False False
key_w = Key 0x57 (Just 'w') False False
key_x = Key 0x58 (Just 'x') False False
key_y = Key 0x59 (Just 'y') False False
key_z = Key 0x5A (Just 'z') False False
key_LEFT = Key vK_LEFT Nothing False False
key_RIGHT = Key vK_RIGHT Nothing False False
-- key_UP = Key vK_UP Nothing False False
-- key_DOWN = Key vK_DOWN Nothing False False
-- key_PGUP = Key vK_PAGE_UP Nothing False False
-- key_PGDOWN = Key vK_PAGE_DOWN Nothing False False
-- key_HOME = Key vK_HOME Nothing False False
-- key_END = Key vK_END Nothing False False
-- key_SPACE = Key vK_SPACE (Just ' ') False False
-- key_TAB = Key vK_TAB Nothing False False
-- key_ENTER = Key vK_ENTER Nothing False False
-- key_BACKSPACE = Key vK_BACK_SPACE Nothing False False
-- key_INSERT = Key vK_INSERT Nothing False False
-- key_RETURN = Key vK_ENTER Nothing False False
-- key_BACK = Key vK_BACK_SPACE Nothing False False
-- key_DELETE = Key vK_DELETE Nothing False False
-- key_DEL = Key vK_DELETE Nothing False False
-- key_LWIN = Key vK_WINDOWS Nothing False False
-- key_APPS = Key vK_CONTEXT_MENU Nothing False False
-- key_POPUP = Key vK_CONTEXT_MENU Nothing False False
-- key_PAUSE = Key vK_PAUSE Nothing False False
-- key_ESCAPE = Key vK_ESCAPE Nothing False False
-- key_MULTIPLY = Key vK_MULTIPLY Nothing False False
-- key_ADD = Key vK_ADD Nothing False False
-- key_SEPARATOR = Key vK_SEPARATOR Nothing False False
-- key_SUBTRACT = Key vK_SUBTRACT Nothing False False
-- key_DECIMAL = Key vK_DECIMAL Nothing False False
-- key_DIVIDE = Key vK_DIVIDE Nothing False False
-- key_NUMPAD0 = Key vK_NUMPAD0 Nothing False False
-- key_NUMPAD0_2 = Key vK_NUMPAD0 Nothing False False
-- key_NUMPAD1 = Key vK_NUMPAD1 Nothing False False
-- key_NUMPAD1_2 = Key vK_NUMPAD1 Nothing False False
-- key_NUMPAD2 = Key vK_NUMPAD2 Nothing False False
-- key_NUMPAD2_2 = Key vK_NUMPAD2 Nothing False False
-- key_NUMPAD3 = Key vK_NUMPAD3 Nothing False False
-- key_NUMPAD3_2 = Key vK_NUMPAD3 Nothing False False
-- key_NUMPAD4 = Key vK_NUMPAD4 Nothing False False
-- key_NUMPAD4_2 = Key vK_NUMPAD4 Nothing False False
-- key_NUMPAD5 = Key vK_NUMPAD5 Nothing False False
-- key_NUMPAD5_2 = Key vK_NUMPAD5 Nothing False False
-- key_NUMPAD6 = Key vK_NUMPAD6 Nothing False False
-- key_NUMPAD6_2 = Key vK_NUMPAD6 Nothing False False
-- key_NUMPAD7 = Key vK_NUMPAD7 Nothing False False
-- key_NUMPAD7_2 = Key vK_NUMPAD7 Nothing False False
-- key_NUMPAD8 = Key vK_NUMPAD8 Nothing False False
-- key_NUMPAD8_2 = Key vK_NUMPAD8 Nothing False False
-- key_NUMPAD9 = Key vK_NUMPAD9 Nothing False False
-- key_NUMPAD9_2 = Key vK_NUMPAD9 Nothing False False
-- key_F1 = Key vK_F1 Nothing False False
-- key_F2 = Key vK_F2 Nothing False False
-- key_F3 = Key vK_F3 Nothing False False
-- key_F4 = Key vK_F4 Nothing False False
-- key_F5 = Key vK_F5 Nothing False False
-- key_F6 = Key vK_F6 Nothing False False
-- key_F7 = Key vK_F7 Nothing False False
-- key_F8 = Key vK_F8 Nothing False False
-- key_F9 = Key vK_F9 Nothing False False
-- key_F10 = Key vK_F10 Nothing False False
-- key_F11 = Key vK_F11 Nothing False False
-- key_F12 = Key vK_F12 Nothing False False
-- key_F13 = Key vK_F13 Nothing False False
-- key_F14 = Key vK_F14 Nothing False False
-- key_F15 = Key vK_F15 Nothing False False
-- key_F16 = Key vK_F16 Nothing False False
-- key_F17 = Key vK_F17 Nothing False False
-- key_F18 = Key vK_F18 Nothing False False
-- key_F19 = Key vK_F19 Nothing False False
-- key_F20 = Key vK_F20 Nothing False False
-- key_F21 = Key vK_F21 Nothing False False
-- key_F22 = Key vK_F22 Nothing False False
-- key_F23 = Key vK_F23 Nothing False False
-- key_F24 = Key vK_F24 Nothing False False
-- key_BANG = Key vK_1 (Just '!') True False
-- key_EXPCLAMATION = Key vK_1 (Just '!') True False
-- key_AT = Key vK_2 (Just '@') True False
-- key_HASH = Key vK_3 (Just '#') True False
-- key_DOLLAR = Key vK_4 (Just '$') True False
-- key_PERCENT = Key vK_5 (Just '%') True False
-- key_CARET = Key vK_6 (Just '^') True False
-- key_AND = Key vK_7 (Just '&') True False
-- key_AMPERSAND = Key vK_7 (Just '&') True False
-- key_STAR = Key vK_8 (Just '*') True False
-- key_ASTERISK = Key vK_8 (Just '*') True False
-- key_LEFT_PAREN = Key vK_9 (Just '(') True False
-- key_L_PAREN = Key vK_9 (Just '(') True False
-- key_RIGHT_PAREN = Key vK_0 (Just ')') True False
-- key_R_PAREN = Key vK_0 (Just ')') True False
-- key_MINUS = Key vK_MINUS (Just '-') False False
-- key_HYPEN = Key vK_MINUS (Just '-') False False
-- key_UNDERSCORE = Key vK_MINUS (Just '_') True False
-- key_PLUS = Key vK_EQUALS (Just '+') True False
-- key_EQUAL = Key vK_EQUALS (Just '=') False False
-- key_EQUALS = Key vK_EQUALS (Just '=') False False
-- key_BACKTICK = Key vK_BACK_QUOTE (Just '`') False False
-- key_TILDE = Key vK_BACK_QUOTE (Just '~') True False
-- key_LEFT_BRACKET = Key vK_OPEN_BRACKET (Just '[') False False
-- key_L_BRACKET = Key vK_OPEN_BRACKET (Just '[') False False
-- key_RIGHT_BRACKET = Key vK_CLOSE_BRACKET (Just ']') False False
-- key_R_BRACKET = Key vK_CLOSE_BRACKET (Just ']') False False
-- key_LEFT_BRACE = Key vK_OPEN_BRACKET (Just '{') True False
-- key_L_BRACE = Key vK_OPEN_BRACKET (Just '{') True False
-- key_RIGHT_BRACE = Key vK_CLOSE_BRACKET (Just '}') True False
-- key_R_BRACE = Key vK_CLOSE_BRACKET (Just '}') True False
-- key_BACKSLASH = Key vK_BACK_SLASH (Just '\\') False False
-- key_BAR = Key vK_BACK_SLASH (Just '|') True False
-- key_COLON = Key vK_SEMICOLON (Just ':') True False
-- key_SEMICOLON = Key vK_SEMICOLON (Just ';') False False
-- key_APOSTROPHE = Key vK_QUOTE (Just '\'') False False
-- key_SINGLE_QUOTE = Key vK_QUOTE (Just '\'') False False
-- key_SQUOTE = Key vK_QUOTE (Just '\'') False False
-- key_QUOTE = Key vK_QUOTE (Just '"') True False
-- key_DOUBLE_QUOTE = Key vK_QUOTE (Just '"') True False
-- key_DQUOTE = Key vK_QUOTE (Just '"') True False
-- key_COMMA = Key vK_COMMA (Just ',') False False
-- key_DOT = Key vK_PERIOD (Just '.') False False
-- key_SLASH = Key vK_SLASH (Just '/') False False
-- key_LESS_THAN = Key vK_COMMA (Just '<') True False
-- key_LEFT_ANGLE = Key vK_COMMA (Just '<') True False
-- key_LANGLE = Key vK_COMMA (Just '<') True False
-- key_GREATER_THAN = Key vK_PERIOD (Just '>') True False
-- key_RIGHT_ANGLE = Key vK_PERIOD (Just '>') True False
-- key_RANGLE = Key vK_PERIOD (Just '>') True False
-- key_QUESTION = Key vK_SLASH (Just '?') True False

keyPress :: Key -> IO ()
keyPress k = keyDown k >> keyUp k

keyUp :: Key -> IO ()
keyUp k = key k True

keyDown :: Key -> IO ()
keyDown k = key k False

withKeyPress :: Key -> IO () -> IO ()
withKeyPress k task = keyDown k >> finally (keyUp k) task

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

