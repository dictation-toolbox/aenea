import json
import os

_tried = set()
_configuration = {'project_root': 'C:\\NatLink\\NatLink\\MacroSystem'}

# Recursively load the config file until we hit a self loop.
while _configuration['project_root'] not in _tried:
    _tried.add(_configuration['project_root'])
    _configuration.update(json.load(open(os.path.join(_configuration['project_root'], 'aenea.json'))))

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
