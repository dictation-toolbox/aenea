"""
Drop this script into ./server/linux_x11/ and run it to benchmark server
functions.
"""
from functools import partial
from server_x11 import get_context, write_text, key_press
import time
import string
import xdo

libxdo = xdo.Xdo()


def bench_function(func, count=100):
    start = time.time()
    for _ in range(0, count):
        func()
    return (time.time() - start) / count


get_context_time = bench_function(get_context)

WRITE_TEXT_STRING = ''.join(string.ascii_lowercase)
write_text_time = bench_function(partial(write_text, WRITE_TEXT_STRING))

key_press_time = bench_function(
    partial(key_press, key='1', modifiers=('control',), count=5)
)

print '\n\n'
print 'running benchmarks...'
print 'get_context %.6f' % get_context_time
print 'write_text %.6f' % write_text_time
print 'key_press %.6f' % key_press_time
print 'done!'
