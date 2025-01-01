// Tags: JDK1.4

// Copyright (C) 2005 Free Software Foundation, Inc.
// Written by Tom Tromey <tromey@redhat.com>,
//            Dalibor Topic <robilad@kaffe.org>

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.io.File;

import java.io.File;
import java.net.URL;
import java.net.MalformedURLException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class UnicodeURI implements Testlet
{
  private static final String LATIN_SMALL_LETTER_C_WITH_ACUTE = "\u0107";
  private static final String fname =  "/tmp/" + LATIN_SMALL_LETTER_C_WITH_ACUTE;

  public void check (TestHarness harness, URL url)
  {
    final String protocol = url.getProtocol();
    final String file = url.getFile();

    harness.verbose("protocol: " + protocol);
    harness.verbose("file: " + file);

    harness.check(protocol, "file");
    harness.check(file, fname);
  }

  public void test (TestHarness harness)
  {
    File f = new File(fname);
    harness.checkPoint("toURL");
    try
      {
	check(harness, f.toURL());
      }
    catch (MalformedURLException _)
      {
	harness.debug(_);
	harness.check(false);
      }

    harness.checkPoint("toURI");
    try
      {
	check(harness, f.toURI().toURL());
      }
    catch (MalformedURLException _)
      {
	harness.debug(_);
	harness.check(false);
      }
  }
}
