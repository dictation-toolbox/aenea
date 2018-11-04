=================
Hacking
=================

Travis CI
---------

We currently have Travis CI set up to run the client unit tests on every PR,
and also run pyflakes on all Python files except for a small set of excluded
files.

The unit test coverage is not comprehensive, and does not exercise the server
at all, so this should not be depended on exclusively -- manual testing is,
unfortunately, also required. But hopefully this will make it a bit easier for
us to prevent bit rot of the tests and catch any small issues pyflakes may
find.

Running the integration tests
-----------------------------

There's an automated integration test for the Linux X11 server. Note that this
sends live commands to a live server, meaning your mouse will jump around and
it will type some text. I've tried to make this as low risk as possible for
what it is, but be cautious, or run in Xnest to be safe.::

    cd server/linux_x11
    cp config.py.example config.py  # Also may need to change the IP to localhost
    python server_x11.py

    # In another window
    python test-client.py

These tests do not currently run in CI, and there's no automated check for
success/failure -- you should just look at it to verify the correct actions
occurred.

It would be awesome to make this safer, more automated, and add to CI, but this
is where we are at this time.

Running the client unit tests
-----------------------------

These tests run in Travis CI automatically. To run them locally:::

    export PYTHONPATH=client
    python test_runner.py `ls client/test/test_*.py`

It'd be nice to get this onto something more standardized, but this works for
now.

Code style
----------

Try to conform to PEP8 where possible. When editing existing code, please keep
style changes in a separate commit from logic changes (small changes ok). All
new code must be pyflakes clean (enforced in CI).
