"""
Example config file for the test driver to supply the pre/post-flight functions to the test suite.
The pre-flight code gets executed during setUp() after the debugger instance is avilable and
the post-flight code gets executed during tearDown() after the debugger instance has done killing
the inferior and deleting all the target programs.

See lldb/test/dotest.py and lldbtest.py which work together to provide config-able pre/post-flight capabilities.
See also lldb/examples/test/usage-pre-post-flight for a usage scenario.
"""

import os
import unittest2

def pre_flight(test):
    __import__("lldb")
    __import__("lldbtest")
    __import__("pexpect")
    print "\nRunning pre-flight function:"
    print "for test case:", test
    # Examples:
    #test.runCmd('platform select remote-macosx')
    #test.runCmd('platform connect -c /tmp/cache connect://localhost:12345 -r -R "-avz"')
    lldbtest_local_cache = os.environ['LLDBTEST_LOCAL_CACHE'] if 'LLDBTEST_LOCAL_CACHE' in os.environ else '/tmp/cache'
    lldbtest_platform = os.environ['LLDBTEST_PLATFORM'] if 'LLDBTEST_PLATFORM' in os.environ else 'remote-macosx'
    lldbtest_platform_url = os.environ['LLDBTEST_PLATFORM_URL'] if 'LLDBTEST_PLATFORM_URL' in os.environ else 'connect://localhost:12345 -r -R "-avz"'
    test.runCmd('platform select %s' % lldbtest_platform)
    test.runCmd('platform connect %s -c %s' % (lldbtest_platform_url, lldbtest_local_cache))

def post_flight(test):
    __import__("lldb")
    __import__("lldbtest")
    __import__("pexpect")
    print "\nRunning post-flight function:"
    print "for test case:", test
    test.runCmd('platform disconnect')

lldbtest_remote_sandbox = os.environ['LLDBTEST_REMOTE_SANDBOX'] if 'LLDBTEST_REMOTE_SANDBOX' in os.environ else None
