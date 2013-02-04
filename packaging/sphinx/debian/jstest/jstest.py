#!/usr/bin/python
# encoding=UTF-8

# Copyright © 2011 Jakub Wilk <jwilk@debian.org>
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

import threading
import urlparse
import urllib
import re
import unittest

from BaseHTTPServer import HTTPServer
from SimpleHTTPServer import SimpleHTTPRequestHandler

import gobject
import gtk
import webkit
import os

default_time_limit = 10.0
time_step = 0.1

# HTTP browser
# ============

class Timeout(Exception):
    pass

class Browser(object):

    def __init__(self, options):
        self._time_limit = 0
        self._view = webkit.WebView()
        self._result = None

    def wget(self, url, time_limit=default_time_limit):
        self._view.open(url)
        self._time_limit = time_limit
        gobject.timeout_add(int(1000 * time_step), self._check)
        gtk.main()
        if self._result is None:
            raise Timeout
        return self._result

    def _extract_html(self):
        # There is not obvious way to extract HTML from WebKit.
        # We use a trick from <http://code.google.com/p/pywebkitgtk/wiki/HowDoI> instead.
        self._view.execute_script('document.title = document.documentElement.innerHTML')
        try:
            return self._view.get_main_frame().get_title() or ''
        finally:
            self._view.execute_script('document.title = ""')

    def _check(self):
        contents = self._extract_html()
        match = re_done.search(contents)
        if match is not None:
            self._result = contents
            gtk.main_quit()
            return
        self._time_limit -= time_step
        if self._time_limit < 0:
            self._result = None
            gtk.main_quit()
        else:
            gobject.timeout_add(int(1000 * time_step), self._check)

# HTTP server
# ===========

class RequestHandler(SimpleHTTPRequestHandler):

    def log_message(*args, **kwargs):
        pass

class TinyHTTPServer(object):

    def __init__(self, directory):
        os.chdir(directory)
        self._httpd = HTTPServer(('127.0.0.1', 0), RequestHandler)
        thread = threading.Thread(target=self._httpd.serve_forever)
        thread.daemon = True
        thread.start()
        host, port = self._httpd.socket.getsockname()
        self.base_url = 'http://%s:%s/' % (host, port)

    def close(self):
        if self._httpd is None:
            return
        self._httpd.shutdown()
        self._httpd = None

    def __enter__(self):
        return self

    def __exit__(self, extype, value, traceback):
        self.close()

# Actual tests
# ============

re_done = re.compile(r'Search finished, found ([0-9]+) page')
re_link = re.compile(r'<a href="[^"]+?highlight=[^"?]+">')
re_highlight = re.compile(r'<span class="highlighted">')

def test_html(html, options):

    class TestCase(unittest.TestCase):

        if options.n_results is not None:
            def test_n_results(self):
                match = re_done.search(html)
                self.assertFalse(match is None)
                n_results = int(match.group(1))
                self.assertEqual(n_results, options.n_results)

        if options.n_links is not None:
            def test_n_links(self):
                matches = re_link.findall(html)
                n_links = len(matches)
                self.assertEqual(n_links, options.n_links)

        if options.n_highlights is not None:
            def test_n_highlights(self):
                matches = re_highlight.findall(html)
                n_highlights = len(matches)
                self.assertEqual(n_highlights, options.n_highlights)

    TestCase.__name__ = 'TestCase(%r)' % options.search_term

    suite = unittest.TestLoader().loadTestsFromTestCase(TestCase)
    return unittest.TextTestRunner(verbosity=2).run(suite)

def test_directory(directory, options, time_limit=default_time_limit):
    # The file:/// protocol doesn't work for the time being:
    # https://bitbucket.org/birkenfeld/sphinx/issue/723/search-broken-in-webkit-based-browsers
    # To work around this problem, spawn our own HTTP server.
    with TinyHTTPServer(directory) as http_server:
        url = urlparse.urljoin(http_server.base_url, 'search.html?q=' + urllib.quote_plus(options.search_term))
        browser = Browser(options)
        html = browser.wget(url, time_limit)
        return test_html(html, options)

def main():
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--time-limit', type=float, default=default_time_limit)
    parser.add_argument('directory', metavar='DIRECTORY')
    parser.add_argument('search_term', metavar='SEARCH-TERM')
    parser.add_argument('--n-results', type=int)
    parser.add_argument('--n-links', type=int)
    parser.add_argument('--n-highlights', type=int)
    options = parser.parse_args()
    test_directory(options.directory, options=options, time_limit=time_limit)

if __name__ == '__main__':
    main()

# vim:ts=4 sw=4 et
