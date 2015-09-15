{-# LANGUAGE ForeignFunctionInterface,
             CPP,
             OverloadedStrings #-}

module Windows ( Key
               , Direction (..)
               , nameToKey
               , charToKey
               , keyAction
               , keyPress
               , withKeyPress
               , getForegroundWindowText
               , getForegroundWindowAncestorText) where

import System.Win32.Types (BYTE, DWORD, UINT, LPTSTR, peekTString)
import qualified Graphics.Win32.Key as W
import Graphics.Win32.GDI.Types (HWND)
import Foreign hiding (shift)
import Foreign.C.Types (CInt (..))
import Data.Text (Text)
import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Map as M
import Control.Applicative (empty)
import Control.Monad (when)
import Control.Exception (finally)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

#include "Windows.h"
#include "Winuser.h"

data Key = Key { keyCode :: W.VKey
               , keyNames :: [Text]
               , keyCharacter :: Maybe Char
               , keyRequiresShift :: Bool }

nameToKey key = M.lookup key keyMap
    where keyMap = M.fromList $ concatMap (\k -> zip (keyNames k) (repeat k)) keys

charToKey :: Char -> Maybe Key
charToKey char = M.lookup char keyMap
    where keyMap = M.fromList $ concatMap pair keys
          pair key = maybe [] (\c -> [(c, key)]) (keyCharacter key)

data Direction = Press | Down | Up

instance FromJSON Direction where
    parseJSON "up" = return Up
    parseJSON "down" = return Down
    parseJSON "press" = return Press
    parseJSON _ = empty

keyAction :: Direction -> Key -> IO ()
keyAction Press = keyPress
keyAction Down = keyDown
keyAction Up = keyUp

keyPress :: Key -> IO ()
keyPress k = keyDown k >> keyUp k

keyUp :: Key -> IO ()
keyUp k = keyCodeAction code False >> when (keyRequiresShift k) (shift False)
    where code = keyCode k

keyDown :: Key -> IO ()
keyDown k = when (keyRequiresShift k) (shift True) >> keyCodeAction code True
    where code = keyCode k

withKeyPress :: Key -> IO () -> IO ()
withKeyPress k task = keyDown k >> task `finally` keyUp k

shift :: Bool -> IO ()
shift = keyCodeAction $ keyCode key_SHIFT

keyCodeAction :: W.VKey -> Bool -> IO ()
keyCodeAction code isDown = let direction = if isDown then 0 else (#const KEYEVENTF_KEYUP)
                                c = fromIntegral code
                            in c_keybd_event c 0 direction 0

getForegroundWindowText :: IO (Maybe String)
getForegroundWindowText = getForegroundWindow >>= maybe (return Nothing) getWindowText

getForegroundWindowAncestorText :: IO (Maybe String)
getForegroundWindowAncestorText = getForegroundWindowAncestor >>= maybe (return Nothing) getWindowText

getWindowText :: HWND -> IO (Maybe String)
getWindowText h = do
  lengthWithNull <- (+1) <$> c_GetWindowTextLength h
  allocaArray (fromIntegral lengthWithNull) $ \textPtr ->
      c_GetWindowText h textPtr lengthWithNull >>= \len ->
      if len > 0
      then Just <$> peekTString textPtr
      else return Nothing

getForegroundWindow :: IO (Maybe HWND)
getForegroundWindow = f <$> c_GetForegroundWindow
    where f ptr = if ptr == nullPtr then Nothing else Just ptr

getForegroundWindowAncestor :: IO (Maybe HWND)
getForegroundWindowAncestor = do
    w <- f <$> c_GetForegroundWindow
    case w of
      Nothing -> return Nothing
      Just w' -> f <$> c_GetAncestor w' (#const GA_ROOTOWNER)
  where f ptr = if ptr == nullPtr then Nothing else Just ptr

foreign import stdcall unsafe "Winuser.h keybd_event"
        c_keybd_event :: BYTE
                      -> BYTE
                      -> DWORD
                      -> DWORD
                      -> IO ()

foreign import stdcall unsafe "Winuser.h GetAncestor"
        c_GetAncestor :: HWND -> UINT -> IO HWND

foreign import stdcall unsafe "Winuser.h GetForegroundWindow"
        c_GetForegroundWindow :: IO HWND

foreign import stdcall unsafe "Winuser.h GetWindowTextLengthW"
        c_GetWindowTextLength :: HWND -> IO CInt

foreign import stdcall unsafe "Winuser.h GetWindowTextW"
        c_GetWindowText :: HWND -> LPTSTR -> CInt -> IO CInt

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
       , key_RIGHT_PAREN
       , key_MINUS
       , key_UNDERSCORE
       , key_PLUS
       , key_EQUAL
       , key_BACKTICK
       , key_TILDE
       , key_LEFT_BRACKET
       , key_RIGHT_BRACKET
       , key_LEFT_BRACE
       , key_RIGHT_BRACE
       , key_BACKSLASH
       , key_BAR
       , key_COLON
       , key_SEMICOLON
       , key_APOSTROPHE
       , key_QUOTE
       , key_COMMA
       , key_DOT
       , key_LESS_THAN
       , key_GREATER_THAN
       , key_SLASH
       , key_QUESTION ]

key_ALT = Key W.vK_MENU ["alt"] Nothing False
key_CONTROL = Key W.vK_CONTROL ["ctrl", "control", "Control_R"] Nothing False
key_SHIFT = Key W.vK_SHIFT ["shift"] Nothing False
key_0 = Key 0x30 ["0"] (Just '0') False
key_1 = Key 0x31 ["1"] (Just '1') False
key_2 = Key 0x32 ["2"] (Just '2') False
key_3 = Key 0x33 ["3"] (Just '3') False
key_4 = Key 0x34 ["4"] (Just '4') False
key_5 = Key 0x35 ["5"] (Just '5') False
key_6 = Key 0x36 ["6"] (Just '6') False
key_7 = Key 0x37 ["7"] (Just '7') False
key_8 = Key 0x38 ["8"] (Just '8') False
key_9 = Key 0x39 ["9"] (Just '9') False
key_A = Key 0x41 ["A"] (Just 'A') True
key_B = Key 0x42 ["B"] (Just 'B') True
key_C = Key 0x43 ["C"] (Just 'C') True
key_D = Key 0x44 ["D"] (Just 'D') True
key_E = Key 0x45 ["E"] (Just 'E') True
key_F = Key 0x46 ["F"] (Just 'F') True
key_G = Key 0x47 ["G"] (Just 'G') True
key_H = Key 0x48 ["H"] (Just 'H') True
key_I = Key 0x49 ["I"] (Just 'I') True
key_J = Key 0x4A ["J"] (Just 'J') True
key_K = Key 0x4B ["K"] (Just 'K') True
key_L = Key 0x4C ["L"] (Just 'L') True
key_M = Key 0x4D ["M"] (Just 'M') True
key_N = Key 0x4E ["N"] (Just 'N') True
key_O = Key 0x4F ["O"] (Just 'O') True
key_P = Key 0x50 ["P"] (Just 'P') True
key_Q = Key 0x51 ["Q"] (Just 'Q') True
key_R = Key 0x52 ["R"] (Just 'R') True
key_S = Key 0x53 ["S"] (Just 'S') True
key_T = Key 0x54 ["T"] (Just 'T') True
key_U = Key 0x55 ["U"] (Just 'U') True
key_V = Key 0x56 ["V"] (Just 'V') True
key_W = Key 0x57 ["W"] (Just 'W') True
key_X = Key 0x58 ["X"] (Just 'X') True
key_Y = Key 0x59 ["Y"] (Just 'Y') True
key_Z = Key 0x5A ["Z"] (Just 'Z') True
key_a = Key 0x41 ["a"] (Just 'a') False
key_b = Key 0x42 ["b"] (Just 'b') False
key_c = Key 0x43 ["c"] (Just 'c') False
key_d = Key 0x44 ["d"] (Just 'd') False
key_e = Key 0x45 ["e"] (Just 'e') False
key_f = Key 0x46 ["f"] (Just 'f') False
key_g = Key 0x47 ["g"] (Just 'g') False
key_h = Key 0x48 ["h"] (Just 'h') False
key_i = Key 0x49 ["i"] (Just 'i') False
key_j = Key 0x4A ["j"] (Just 'j') False
key_k = Key 0x4B ["k"] (Just 'k') False
key_l = Key 0x4C ["l"] (Just 'l') False
key_m = Key 0x4D ["m"] (Just 'm') False
key_n = Key 0x4E ["n"] (Just 'n') False
key_o = Key 0x4F ["o"] (Just 'o') False
key_p = Key 0x50 ["p"] (Just 'p') False
key_q = Key 0x51 ["q"] (Just 'q') False
key_r = Key 0x52 ["r"] (Just 'r') False
key_s = Key 0x53 ["s"] (Just 's') False
key_t = Key 0x54 ["t"] (Just 't') False
key_u = Key 0x55 ["u"] (Just 'u') False
key_v = Key 0x56 ["v"] (Just 'v') False
key_w = Key 0x57 ["w"] (Just 'w') False
key_x = Key 0x58 ["x"] (Just 'x') False
key_y = Key 0x59 ["y"] (Just 'y') False
key_z = Key 0x5A ["z"] (Just 'z') False
key_LEFT = Key W.vK_LEFT ["left"] Nothing False
key_RIGHT = Key W.vK_RIGHT ["right"] Nothing False
key_UP = Key W.vK_UP ["up"] Nothing False
key_DOWN = Key W.vK_DOWN ["down"] Nothing False
key_PGUP = Key W.vK_PRIOR ["pgup"] Nothing False
key_PGDOWN = Key W.vK_NEXT ["pgdown"] Nothing False
key_HOME = Key W.vK_HOME ["home"] Nothing False
key_END = Key W.vK_END ["end"] Nothing False
key_SPACE = Key W.vK_SPACE ["space"] (Just ' ') False
key_TAB = Key W.vK_TAB ["tab"] (Just '\t') False
key_ENTER = Key W.vK_RETURN ["enter", "return"] (Just '\n') False
key_BACKSPACE = Key W.vK_BACK ["back", "backspace"] Nothing False
key_INSERT = Key W.vK_INSERT ["insert"] Nothing False
key_DELETE = Key W.vK_DELETE ["delete"] Nothing False
key_DEL = Key W.vK_DELETE ["del"] Nothing False
key_LWIN = Key (#const VK_LWIN) ["win"] Nothing False
key_APPS = Key W.vK_MENU ["apps", "popup"] Nothing False
key_PAUSE = Key W.vK_PAUSE ["pause"] Nothing False
key_ESCAPE = Key W.vK_ESCAPE ["escape"] Nothing False
key_MULTIPLY = Key W.vK_MULTIPLY ["npmul"] Nothing False
key_ADD = Key W.vK_ADD ["npadd"] Nothing False
key_SEPARATOR = Key W.vK_SEPARATOR ["npsep"] Nothing False
key_SUBTRACT = Key W.vK_SUBTRACT ["npsub"] Nothing False
key_DECIMAL = Key W.vK_DECIMAL ["npdec"] Nothing False
key_DIVIDE = Key W.vK_DIVIDE ["npdiv"] Nothing False
key_NUMPAD0 = Key W.vK_NUMPAD0 ["numpad0", "np0"] Nothing False
key_NUMPAD1 = Key W.vK_NUMPAD1 ["numpad1", "np1"] Nothing False
key_NUMPAD2 = Key W.vK_NUMPAD2 ["numpad2", "np2"] Nothing False
key_NUMPAD3 = Key W.vK_NUMPAD3 ["numpad3", "np3"] Nothing False
key_NUMPAD4 = Key W.vK_NUMPAD4 ["numpad4", "np4"] Nothing False
key_NUMPAD5 = Key W.vK_NUMPAD5 ["numpad5", "np5"] Nothing False
key_NUMPAD6 = Key W.vK_NUMPAD6 ["numpad6", "np6"] Nothing False
key_NUMPAD7 = Key W.vK_NUMPAD7 ["numpad7", "np7"] Nothing False
key_NUMPAD8 = Key W.vK_NUMPAD8 ["numpad8", "np8"] Nothing False
key_NUMPAD9 = Key W.vK_NUMPAD9 ["numpad9", "np9"] Nothing False
key_F1 = Key W.vK_F1 ["f1"] Nothing False
key_F2 = Key W.vK_F2 ["f2"] Nothing False
key_F3 = Key W.vK_F3 ["f3"] Nothing False
key_F4 = Key W.vK_F4 ["f4"] Nothing False
key_F5 = Key W.vK_F5 ["f5"] Nothing False
key_F6 = Key W.vK_F6 ["f6"] Nothing False
key_F7 = Key W.vK_F7 ["f7"] Nothing False
key_F8 = Key W.vK_F8 ["f8"] Nothing False
key_F9 = Key W.vK_F9 ["f9"] Nothing False
key_F10 = Key W.vK_F10 ["f10"] Nothing False
key_F11 = Key W.vK_F11 ["f11"] Nothing False
key_F12 = Key W.vK_F12 ["f12"] Nothing False
key_F13 = Key W.vK_F13 ["f13"] Nothing False
key_F14 = Key W.vK_F14 ["f14"] Nothing False
key_F15 = Key W.vK_F15 ["f15"] Nothing False
key_F16 = Key W.vK_F16 ["f16"] Nothing False
key_F17 = Key W.vK_F17 ["f17"] Nothing False
key_F18 = Key W.vK_F18 ["f18"] Nothing False
key_F19 = Key W.vK_F19 ["f19"] Nothing False
key_F20 = Key W.vK_F20 ["f20"] Nothing False
key_F21 = Key W.vK_F21 ["f21"] Nothing False
key_F22 = Key W.vK_F22 ["f22"] Nothing False
key_F23 = Key W.vK_F23 ["f23"] Nothing False
key_F24 = Key W.vK_F24 ["f24"] Nothing False
key_EXCLAMATION = Key (keyCode key_1) ["exclamation", "bang", "exclam"] (Just '!') True
key_AT = Key (keyCode key_2) ["at"] (Just '@') True
key_HASH = Key (keyCode key_3) ["hash", "numbersign"] (Just '#') True
key_DOLLAR = Key (keyCode key_4) ["dollar"] (Just '$') True
key_PERCENT = Key (keyCode key_5) ["percent"] (Just '%') True
key_CARET = Key (keyCode key_6) ["caret", "asciicircum"] (Just '^') True
key_AMPERSAND = Key (keyCode key_7) ["ampersand", "and"] (Just '&') True
key_ASTERISK = Key (keyCode key_8) ["asterisk", "star"] (Just '*') True
key_LEFT_PAREN = Key (keyCode key_9) ["leftparen", "lparen", "parenleft"] (Just '(') True
key_RIGHT_PAREN = Key (keyCode key_0) ["rightparen", "rparen", "parenright"] (Just ')') True
key_MINUS = Key 0xBD ["hyphen", "minus"] (Just '-') False
key_UNDERSCORE = Key 0xBD ["underscore"] (Just '_') True
key_PLUS = Key 0xBB ["plus"] (Just '+') True
key_EQUAL = Key 0xBB ["equal", "equals"] (Just '=') False
key_BACKTICK = Key (#const VK_OEM_3) ["backtick", "quoteleft"] (Just '`') False
key_TILDE = Key (#const VK_OEM_3) ["tilde"] (Just '~') True
key_LEFT_BRACKET = Key (#const VK_OEM_4) ["leftbracket", "lbracket", "bracketleft"] (Just '[') False
key_RIGHT_BRACKET = Key (#const VK_OEM_6) ["rightbracket", "rbracket", "bracketright"] (Just ']') False
key_LEFT_BRACE = Key (#const VK_OEM_4) ["leftbrace", "lbrace", "braceleft"] (Just '{') True
key_RIGHT_BRACE = Key (#const VK_OEM_6) ["rightbrace", "rbrace", "braceright"] (Just '}') True
key_BACKSLASH = Key (#const VK_OEM_5) ["backslash"] (Just '\\') False
key_BAR = Key (#const VK_OEM_5) ["bar"] (Just '|') True
key_COLON = Key (#const VK_OEM_1) ["colon"] (Just ':') True
key_SEMICOLON = Key (#const VK_OEM_1) ["semicolon"] (Just ';') False
key_APOSTROPHE = Key (#const VK_OEM_7) ["apostrophe", "singlequote", "squote"] (Just '\'') False
key_QUOTE = Key (#const VK_OEM_7) ["quote", "doublequote", "dquote"] (Just '"') True
key_COMMA = Key 0xBC ["comma"] (Just ',') False
key_DOT = Key 0xBE ["dot", "period"] (Just '.') False
key_LESS_THAN = Key 0xBC ["lessthan", "leftangle", "langle", "less"] (Just '<') True
key_GREATER_THAN = Key 0xBE ["greaterthan", "rightangle", "rangle", "greater"] (Just '>') True
key_SLASH = Key (#const VK_OEM_2) ["slash"] (Just '/') False
key_QUESTION = Key (#const VK_OEM_2) ["question"] (Just '?') True
