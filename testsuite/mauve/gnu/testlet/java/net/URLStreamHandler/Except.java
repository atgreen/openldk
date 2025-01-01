/* Except.java -- Regression test for URLStreamHandler
   Copyright (C) 2006 Red Hat, Inc.
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.4

package gnu.testlet.java.net.URLStreamHandler;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLStreamHandler;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * PR 28095 regression test.
 */
public class Except implements Testlet {
	public static class Handler extends URLStreamHandler
	{
		protected URLConnection openConnection(URL u)
		{
			return null;
		}

		protected void parseURL(URL url, String spec, int start, int end)
		{
			throw new RuntimeException();
		}
	}

	public void test(TestHarness harness) {
		boolean ok = false;
		try {
			URL u = new URL(null, "blah://", new Handler());
		} catch (MalformedURLException ignore) {
			ok = true;
		} catch (Exception ex) {
			harness.debug(ex);
		}
		harness.check(ok);
	}
}
