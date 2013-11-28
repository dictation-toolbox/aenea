{-# LANGUAGE ForeignFunctionInterface
, CPP
, GeneralizedNewtypeDeriving
, OverloadedStrings
, TupleSections #-}

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
               , keyNames :: [Text]
               , keyCharacter :: Maybe Char
               , keyRequiresShift :: Bool
               , keyIsModifier :: Bool }
         deriving (Eq)

keys = [ key_ALT
       , key_CONTROL
       , key_SHIFT
       , key_0
       , key_1
       , key_2
       , key_3
       , key_4
       , key_5
       , key_6
       , key_7
       , key_8
       , key_9
       , key_A
       , key_B
       , key_C
       , key_D
       , key_E
       , key_F
       , key_G
       , key_H
       , key_I
       , key_J
       , key_K
       , key_L
       , key_M
       , key_N
       , key_O
       , key_P
       , key_Q
       , key_R
       , key_S
       , key_T
       , key_U
       , key_V
       , key_W
       , key_X
       , key_Y
       , key_Z
       , key_a
       , key_b
       , key_c
       , key_d
       , key_e
       , key_f
       , key_g
       , key_h
       , key_i
       , key_j
       , key_k
       , key_l
       , key_m
       , key_n
       , key_o
       , key_p
       , key_q
       , key_r
       , key_s
       , key_t
       , key_u
       , key_v
       , key_w
       , key_x
       , key_y
       , key_z
       , key_LEFT
       , key_RIGHT
       , key_UP
       , key_DOWN
       , key_PGUP
       , key_PGDOWN
       , key_HOME
       , key_END
       , key_SPACE
       , key_TAB
       , key_ENTER
       , key_BACKSPACE
       , key_INSERT
       , key_DELETE
       , key_DEL
       , key_LWIN
       , key_APPS
       , key_PAUSE
       , key_ESCAPE
       , key_MULTIPLY
       , key_ADD
       , key_SEPARATOR
       , key_SUBTRACT
       , key_DECIMAL
       , key_DIVIDE
       , key_NUMPAD0
       , key_NUMPAD1
       , key_NUMPAD2
       , key_NUMPAD3
       , key_NUMPAD4
       , key_NUMPAD5
       , key_NUMPAD6
       , key_NUMPAD7
       , key_NUMPAD8
       , key_NUMPAD9
       , key_F1
       , key_F2
       , key_F3
       , key_F4
       , key_F5
       , key_F6
       , key_F7
       , key_F8
       , key_F9
       , key_F10
       , key_F11
       , key_F12
       , key_F13
       , key_F14
       , key_F15
       , key_F16
       , key_F17
       , key_F18
       , key_F19
       , key_F20
       , key_F21
       , key_F22
       , key_F23
       , key_F24
       , key_EXCLAMATION
       , key_AT
       , key_HASH
       , key_DOLLAR
       , key_PERCENT
       , key_CARET
       , key_AMPERSAND
       , key_ASTERISK
       , key_LEFT_PAREN
       , key_RIGHT_PAREN ]

nameToKey key = lookup key keyMap
                where keyMap = concatMap (\k -> map ( , k) (keyNames k)) keys

key_ALT = Key vK_MENU ["alt"] Nothing False True
key_CONTROL = Key vK_CONTROL ["ctrl", "control"] Nothing False True
key_SHIFT = Key vK_SHIFT ["shift"] Nothing False True
key_0 = Key 0x30 ["0"] (Just '0') False False
key_1 = Key 0x31 ["1"] (Just '1') False False
key_2 = Key 0x31 ["2"] (Just '2') False False
key_3 = Key 0x33 ["3"] (Just '3') False False
key_4 = Key 0x34 ["4"] (Just '4') False False
key_5 = Key 0x35 ["5"] (Just '5') False False
key_6 = Key 0x36 ["6"] (Just '6') False False
key_7 = Key 0x37 ["7"] (Just '7') False False
key_8 = Key 0x38 ["8"] (Just '8') False False
key_9 = Key 0x39 ["9"] (Just '9') False False
key_A = Key 0x41 ["A"] (Just 'A') True False
key_B = Key 0x42 ["B"] (Just 'B') True False
key_C = Key 0x43 ["C"] (Just 'C') True False
key_D = Key 0x44 ["D"] (Just 'D') True False
key_E = Key 0x45 ["E"] (Just 'E') True False
key_F = Key 0x46 ["F"] (Just 'F') True False
key_G = Key 0x47 ["G"] (Just 'G') True False
key_H = Key 0x48 ["H"] (Just 'H') True False
key_I = Key 0x49 ["I"] (Just 'I') True False
key_J = Key 0x4A ["J"] (Just 'J') True False
key_K = Key 0x4B ["K"] (Just 'K') True False
key_L = Key 0x4C ["L"] (Just 'L') True False
key_M = Key 0x4D ["M"] (Just 'M') True False
key_N = Key 0x4E ["N"] (Just 'N') True False
key_O = Key 0x4F ["O"] (Just 'O') True False
key_P = Key 0x50 ["P"] (Just 'P') True False
key_Q = Key 0x51 ["Q"] (Just 'Q') True False
key_R = Key 0x52 ["R"] (Just 'R') True False
key_S = Key 0x53 ["S"] (Just 'S') True False
key_T = Key 0x54 ["T"] (Just 'T') True False
key_U = Key 0x55 ["U"] (Just 'U') True False
key_V = Key 0x56 ["V"] (Just 'V') True False
key_W = Key 0x57 ["W"] (Just 'W') True False
key_X = Key 0x58 ["X"] (Just 'X') True False
key_Y = Key 0x59 ["Y"] (Just 'Y') True False
key_Z = Key 0x5A ["Z"] (Just 'Z') True False
key_a = Key 0x41 ["a"] (Just 'a') False False
key_b = Key 0x42 ["b"] (Just 'b') False False
key_c = Key 0x43 ["c"] (Just 'c') False False
key_d = Key 0x44 ["d"] (Just 'd') False False
key_e = Key 0x45 ["e"] (Just 'e') False False
key_f = Key 0x46 ["f"] (Just 'f') False False
key_g = Key 0x47 ["g"] (Just 'g') False False
key_h = Key 0x48 ["h"] (Just 'h') False False
key_i = Key 0x49 ["i"] (Just 'i') False False
key_j = Key 0x4A ["j"] (Just 'j') False False
key_k = Key 0x4B ["k"] (Just 'k') False False
key_l = Key 0x4C ["l"] (Just 'l') False False
key_m = Key 0x4D ["m"] (Just 'm') False False
key_n = Key 0x4E ["n"] (Just 'n') False False
key_o = Key 0x4F ["o"] (Just 'o') False False
key_p = Key 0x50 ["p"] (Just 'p') False False
key_q = Key 0x51 ["q"] (Just 'q') False False
key_r = Key 0x52 ["r"] (Just 'r') False False
key_s = Key 0x53 ["s"] (Just 's') False False
key_t = Key 0x54 ["t"] (Just 't') False False
key_u = Key 0x55 ["u"] (Just 'u') False False
key_v = Key 0x56 ["v"] (Just 'v') False False
key_w = Key 0x57 ["w"] (Just 'w') False False
key_x = Key 0x58 ["x"] (Just 'x') False False
key_y = Key 0x59 ["y"] (Just 'y') False False
key_z = Key 0x5A ["z"] (Just 'z') False False
key_LEFT = Key vK_LEFT ["left"] Nothing False False
key_RIGHT = Key vK_RIGHT ["right"] Nothing False False
key_UP = Key vK_UP ["up"] Nothing False False
key_DOWN = Key vK_DOWN ["down"] Nothing False False
key_PGUP = Key vK_PRIOR ["pgup"] Nothing False False
key_PGDOWN = Key vK_NEXT ["pgdown"] Nothing False False
key_HOME = Key vK_HOME ["home"] Nothing False False
key_END = Key vK_END ["end"] Nothing False False
key_SPACE = Key vK_SPACE ["space"] (Just ' ') False False
key_TAB = Key vK_TAB ["tab"] Nothing False False
key_ENTER = Key vK_RETURN ["enter", "return"] Nothing False False
key_BACKSPACE = Key vK_BACK ["back", "backspace"] Nothing False False
key_INSERT = Key vK_INSERT ["insert"] Nothing False False
key_DELETE = Key vK_DELETE ["delete"] Nothing False False
key_DEL = Key vK_DELETE ["del"] Nothing False False
key_LWIN = Key (#const VK_LWIN) ["win"] Nothing False False
key_APPS = Key vK_MENU ["apps", "popup"] Nothing False False
key_PAUSE = Key vK_PAUSE ["pause"] Nothing False False
key_ESCAPE = Key vK_ESCAPE ["escape"] Nothing False False
key_MULTIPLY = Key vK_MULTIPLY ["npmul"] Nothing False False
key_ADD = Key vK_ADD ["npadd"] Nothing False False
key_SEPARATOR = Key vK_SEPARATOR ["npsep"] Nothing False False
key_SUBTRACT = Key vK_SUBTRACT ["npsub"] Nothing False False
key_DECIMAL = Key vK_DECIMAL ["npdec"] Nothing False False
key_DIVIDE = Key vK_DIVIDE ["npdiv"] Nothing False False
key_NUMPAD0 = Key vK_NUMPAD0 ["numpad0", "np0"] Nothing False False
key_NUMPAD1 = Key vK_NUMPAD1 ["numpad1", "np1"] Nothing False False
key_NUMPAD2 = Key vK_NUMPAD2 ["numpad2", "np2"] Nothing False False
key_NUMPAD3 = Key vK_NUMPAD3 ["numpad3", "np3"] Nothing False False
key_NUMPAD4 = Key vK_NUMPAD4 ["numpad4", "np4"] Nothing False False
key_NUMPAD5 = Key vK_NUMPAD5 ["numpad5", "np5"] Nothing False False
key_NUMPAD6 = Key vK_NUMPAD6 ["numpad6", "np6"] Nothing False False
key_NUMPAD7 = Key vK_NUMPAD7 ["numpad7", "np7"] Nothing False False
key_NUMPAD8 = Key vK_NUMPAD8 ["numpad8", "np8"] Nothing False False
key_NUMPAD9 = Key vK_NUMPAD9 ["numpad9", "np9"] Nothing False False
key_F1 = Key vK_F1 ["f1"] Nothing False False
key_F2 = Key vK_F2 ["f2"] Nothing False False
key_F3 = Key vK_F3 ["f3"] Nothing False False
key_F4 = Key vK_F4 ["f4"] Nothing False False
key_F5 = Key vK_F5 ["f5"] Nothing False False
key_F6 = Key vK_F6 ["f6"] Nothing False False
key_F7 = Key vK_F7 ["f7"] Nothing False False
key_F8 = Key vK_F8 ["f8"] Nothing False False
key_F9 = Key vK_F9 ["f9"] Nothing False False
key_F10 = Key vK_F10 ["f10"] Nothing False False
key_F11 = Key vK_F11 ["f11"] Nothing False False
key_F12 = Key vK_F12 ["f12"] Nothing False False
key_F13 = Key vK_F13 ["f13"] Nothing False False
key_F14 = Key vK_F14 ["f14"] Nothing False False
key_F15 = Key vK_F15 ["f15"] Nothing False False
key_F16 = Key vK_F16 ["f16"] Nothing False False
key_F17 = Key vK_F17 ["f17"] Nothing False False
key_F18 = Key vK_F18 ["f18"] Nothing False False
key_F19 = Key vK_F19 ["f19"] Nothing False False
key_F20 = Key vK_F20 ["f20"] Nothing False False
key_F21 = Key vK_F21 ["f21"] Nothing False False
key_F22 = Key vK_F22 ["f22"] Nothing False False
key_F23 = Key vK_F23 ["f23"] Nothing False False
key_F24 = Key vK_F24 ["f24"] Nothing False False
key_EXCLAMATION = Key (keyCode key_1) ["exclamation", "bang"] (Just '!') True False
key_AT = Key (keyCode key_2) ["at"] (Just '@') True False
key_HASH = Key (keyCode key_3) ["hash"] (Just '#') True False
key_DOLLAR = Key (keyCode key_4) ["dollar"] (Just '$') True False
key_PERCENT = Key (keyCode key_5) ["percent"] (Just '%') True False
key_CARET = Key (keyCode key_6) ["caret"] (Just '^') True False
key_AMPERSAND = Key (keyCode key_7) ["ampersand", "and"] (Just '&') True False
key_ASTERISK = Key (keyCode key_8) ["asterisk", "star"] (Just '*') True False
key_LEFT_PAREN = Key (keyCode key_9) ["leftparen", "lparen"] (Just '(') True False
key_RIGHT_PAREN = Key (keyCode key_0) ["rightparen", "rparen"] (Just ')') True False
-- key_MINUS = Key (#const VK_OEM_MINUS) ["minus"] (Just '-') False False
-- key_HYPEN = Key vK_MINUS ["hyphen"] (Just '-') False False
-- key_UNDERSCORE = Key vK_MINUS ["underscore"] (Just '_') True False
-- key_PLUS = Key (#const VK_OEM_PLUS) ["plus"] (Just '+') True False
-- key_EQUAL = Key (#const VK_OEM_PLUS) ["equal", "equals"] (Just '=') False False
-- key_BACKTICK = Key vK_BACK_QUOTE ["backtick"] (Just '`') False False
-- key_TILDE = Key vK_BACK_QUOTE ["tilde"] (Just '~') True False
-- key_LEFT_BRACKET = Key vK_OPEN_BRACKET ["leftbracket"] (Just '[') False False
-- key_L_BRACKET = Key vK_OPEN_BRACKET ["lbracket"] (Just '[') False False
-- key_RIGHT_BRACKET = Key vK_CLOSE_BRACKET ["rightbracket"] (Just ']') False False
-- key_R_BRACKET = Key vK_CLOSE_BRACKET ["rbracket"] (Just ']') False False
-- key_LEFT_BRACE = Key vK_OPEN_BRACKET ["leftbrace"] (Just '{') True False
-- key_L_BRACE = Key vK_OPEN_BRACKET ["lbrace"] (Just '{') True False
-- key_RIGHT_BRACE = Key vK_CLOSE_BRACKET ["rightbrace"] (Just '}') True False
-- key_R_BRACE = Key vK_CLOSE_BRACKET ["rbrace"] (Just '}') True False
-- key_BACKSLASH = Key vK_BACK_SLASH ["backslash"] (Just '\\') False False
-- key_BAR = Key vK_BACK_SLASH ["bar"] (Just '|') True False
-- key_COLON = Key vK_SEMICOLON ["colon"] (Just ':') True False
-- key_SEMICOLON = Key vK_SEMICOLON ["semicolon"] (Just ';') False False
-- key_APOSTROPHE = Key vK_QUOTE ["apostrophe"] (Just '\'') False False
-- key_SINGLE_QUOTE = Key vK_QUOTE ["singlequote"] (Just '\'') False False
-- key_SQUOTE = Key vK_QUOTE ["squote"] (Just '\'') False False
-- key_QUOTE = Key vK_QUOTE ["quote"] (Just '"') True False
-- key_DOUBLE_QUOTE = Key vK_QUOTE ["doublequote"] (Just '"') True False
-- key_DQUOTE = Key vK_QUOTE ["dquote"] (Just '"') True False
-- key_COMMA = Key vK_COMMA ["comma"] (Just ',') False False
-- key_DOT = Key vK_PERIOD ["dot"] (Just '.') False False
-- key_SLASH = Key vK_SLASH ["slash"] (Just '/') False False
-- key_LESS_THAN = Key vK_COMMA ["lessthan"] (Just '<') True False
-- key_LEFT_ANGLE = Key vK_COMMA ["leftangle"] (Just '<') True False
-- key_LANGLE = Key vK_COMMA ["langle"] (Just '<') True False
-- key_GREATER_THAN = Key vK_PERIOD ["greaterthan"] (Just '>') True False
-- key_RIGHT_ANGLE = Key vK_PERIOD ["rightangle"] (Just '>') True False
-- key_RANGLE = Key vK_PERIOD ["rangle"] (Just '>') True False
-- key_QUESTION = Key vK_SLASH ["question"] (Just '?') True False

data Direction = Press | Down | Up

keyEvent :: Direction -> Key -> IO ()
keyEvent d k = case d of
                 Press -> keyPress k
                 Down -> keyDown k
                 Up -> keyUp k

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

