import json
import os

try:
    import dragonfly
except ImportError:
    import dragonfly_mock as dragonfly


STARTING_PROJECT_POOT = 'C:\\NatLink\\NatLink\\MacroSystem'

if os.path.exists(os.path.join(STARTING_PROJECT_POOT, 'aenea.json')):
    _configuration = {'project_root': STARTING_PROJECT_POOT}

    _tried = set()
    # Recursively load the config file until we hit a self loop.
    while _configuration['project_root'] not in _tried:
        _tried.add(_configuration['project_root'])
        _configuration.update(json.load(open(os.path.join(_configuration['project_root'], 'aenea.json'))))
else:
    _configuration = {
        'project_root': '.',
        'host': 'localhost',
        'port': 8240,
        'platform': 'proxy',
        'use_multiple_actions': True,
        'screen_resolution': [1920, 1080],
        'keys': ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'A', 'alt', 'Alt_L', 'Alt_R', 'ampersand', 'apostrophe', 'apps', 'asciicircum', 'asciitilde', 'asterisk', 'at', 'b', 'B', 'backslash', 'backspace', 'BackSpace', 'backtick', 'bar', 'braceleft', 'braceright', 'bracketleft', 'bracketright', 'Break', 'brokenbar', 'c', 'C', 'Cancel', 'Caps_Lock', 'caret', 'colon', 'comma', 'Control_L', 'Control_R', 'ctrl', 'd', 'D', 'dead_abovedot', 'dead_acute', 'dead_caron', 'dead_cedilla', 'dead_circumflex', 'dead_diaeresis', 'dead_doubleacute', 'dead_grave', 'dead_ogonek', 'dead_tilde', 'del', 'Delete', 'dollar', 'dot', 'down', 'Down', 'dquote', 'e', 'E', 'end', 'End', 'enter', 'equal', 'escape', 'Escape', 'exclam', 'exclamation', 'f', 'F', 'f1', 'F1', 'f10', 'F10', 'f11', 'F11', 'f12', 'F12', 'f13', 'f14', 'f15', 'f16', 'f17', 'f18', 'f19', 'f2', 'F2', 'f20', 'f21', 'f22', 'f23', 'f24', 'f3', 'F3', 'f4', 'F4', 'f5', 'F5', 'f6', 'F6', 'f7', 'F7', 'f8', 'F8', 'f9', 'F9', 'Find', 'g', 'G', 'grave', 'greater', 'h', 'H', 'Hangul', 'Hangul_Hanja', 'hash', 'Help', 'Henkan_Mode', 'Hiragana', 'Hiragana_Katakana', 'home', 'Home', 'Hyper_L', 'Hyper_R', 'hyphen', 'i', 'I', 'insert', 'Insert', 'ISO_Left_Tab', 'ISO_Level3_Shift', 'j', 'J', 'k', 'K', 'Katakana', 'KP_0', 'KP_1', 'KP_2', 'KP_3', 'KP_4', 'KP_5', 'KP_6', 'KP_7', 'KP_8', 'KP_9', 'KP_Add', 'KP_Begin', 'KP_Decimal', 'KP_Delete', 'KP_Divide', 'KP_Down', 'KP_End', 'KP_Enter', 'KP_Equal', 'KP_Home', 'KP_Insert', 'KP_Left', 'KP_Multiply', 'KP_Next', 'KP_Prior', 'KP_Right', 'KP_Subtract', 'KP_Up', 'l', 'L', 'langle', 'lbrace', 'lbracket', 'left', 'Left', 'less', 'Linefeed', 'lparen', 'm', 'M', 'Menu', 'Meta_L', 'Meta_R', 'minus', 'Mode_switch', 'Muhenkan', 'n', 'N', 'Next', 'NoSymbol', 'np0', 'np1', 'np2', 'np3', 'np4', 'np5', 'np6', 'np7', 'np8', 'np9', 'npadd', 'npdec', 'npdiv', 'npmul', 'npsep', 'npsub', 'numbersign', 'Num_Lock', 'o', 'O', 'p', 'P', 'parenleft', 'parenright', 'Pause', 'percent', 'period', 'periodcentered', 'pgdown', 'pgup', 'plus', 'plusminus', 'Print', 'Prior', 'q', 'Q', 'question', 'quotedbl', 'r', 'R', 'rangle', 'rbrace', 'rbracket', 'Redo', 'Return', 'right', 'Right', 'rparen', 's', 'S', 'Scroll_Lock', 'semicolon', 'shift', 'Shift_L', 'Shift_R', 'slash', 'space', 'squote', 'SunFront', 'SunOpen', 'SunProps', 'Super_L', 'Super_R', 'Sys_Req', 't', 'T', 'tab', 'Tab', 'tilde', 'u', 'U', 'underscore', 'Undo', 'up', 'Up', 'v', 'V', 'w', 'W', 'win', 'x', 'X', 'XF86AudioForward', 'XF86AudioLowerVolume', 'XF86AudioMedia', 'XF86AudioMute', 'XF86AudioNext', 'XF86AudioPause', 'XF86AudioPlay', 'XF86AudioPrev', 'XF86AudioRaiseVolume', 'XF86AudioRecord', 'XF86AudioRewind', 'XF86AudioStop', 'XF86Back', 'XF86Battery', 'XF86Bluetooth', 'XF86Calculator', 'XF86ClearGrab', 'XF86Close', 'XF86Copy', 'XF86Cut', 'XF86Display', 'XF86Documents', 'XF86DOS', 'XF86Eject', 'XF86Explorer', 'XF86Favorites', 'XF86Finance', 'XF86Forward', 'XF86Game', 'XF86Go', 'XF86HomePage', 'XF86KbdBrightnessDown', 'XF86KbdBrightnessUp', 'XF86KbdLightOnOff', 'XF86Launch1', 'XF86Launch2', 'XF86Launch3', 'XF86Launch4', 'XF86Launch5', 'XF86Launch6', 'XF86Launch7', 'XF86Launch8', 'XF86Launch9', 'XF86LaunchA', 'XF86LaunchB', 'XF86Mail', 'XF86MailForward', 'XF86MenuKB', 'XF86Messenger', 'XF86MonBrightnessDown', 'XF86MonBrightnessUp', 'XF86MyComputer', 'XF86New', 'XF86Next_VMode', 'XF86Paste', 'XF86Phone', 'XF86PowerOff', 'XF86Prev_VMode', 'XF86Reload', 'XF86Reply', 'XF86RotateWindows', 'XF86Save', 'XF86ScreenSaver', 'XF86ScrollDown', 'XF86ScrollUp', 'XF86Search', 'XF86Send', 'XF86Shop', 'XF86Sleep', 'XF86Suspend', 'XF86Switch_VT_1', 'XF86Switch_VT_10', 'XF86Switch_VT_11', 'XF86Switch_VT_12', 'XF86Switch_VT_2', 'XF86Switch_VT_3', 'XF86Switch_VT_4', 'XF86Switch_VT_5', 'XF86Switch_VT_6', 'XF86Switch_VT_7', 'XF86Switch_VT_8', 'XF86Switch_VT_9', 'XF86Tools', 'XF86TouchpadOff', 'XF86TouchpadOn', 'XF86TouchpadToggle', 'XF86Ungrab', 'XF86WakeUp', 'XF86WebCam', 'XF86WLAN', 'XF86WWW', 'XF86Xfer', 'y', 'Y', 'z', 'Z'],
        'modifiers': {'a': 'alt', 'A': 'Alt_R', 'c': 'control', 'w': 'super', 'h': 'hyper', 'm': 'meta', 'C': 'Control_R', 's': 'shift', 'S': 'Shift_R', 'M': 'Meta_R', 'W': 'Super_R', 'H': 'Hyper_R'},
        }

PROJECT_ROOT = _configuration['project_root']

# Client-side config (Windows running Dragon sending commands)
HOST = _configuration['host']
PORT = _configuration['port']

# Whether to use proxy or native (not all modules support native.)
PLATFORM = _configuration['platform']

# Whether to use the server's multiple_actions RPC method.
USE_MULTIPLE_ACTIONS = _configuration['use_multiple_actions']

SCREEN_RESOLUTION = _configuration['screen_resolution']

KEYS = _configuration.get('keys', [])
MODIFIERS = _configuration.get('modifiers', {})

CONNECT_RETRY_COOLDOWN = _configuration.get('connect_retry_cooldown', 5)

if _configuration.get('restrict_proxy_to_aenea_client', False):
    proxy_enable_context = dragonfly.AppContext(
        executable="python",
        title="Aenea client - Dictation capturing"
        )
else:
    def _scoped():
        class AlwaysContext(dragonfly.Context):
            def matches(self, windows_executable, windows_title, windows_handle):
                return True
        return AlwaysContext()
    proxy_enable_context = _scoped()


def proxy_active(active_window=None):
    '''Returns whether the proxy is enabled, based on context and file
       settings.'''
    if active_window is None:
        active_window = dragonfly.Window.get_foreground()
    return (proxy_enable_context.matches(*active_window) and
            PLATFORM == 'proxy')
