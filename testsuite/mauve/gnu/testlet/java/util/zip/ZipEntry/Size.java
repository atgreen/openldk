// Tags: JDK1.1

// Copyright (C) 2005 Free Software Foundation
// Contributed by Tom Tromey <tromey@redhat.com>

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

package gnu.testlet.java.util.zip.ZipEntry;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.zip.*;

public class Size implements Testlet
{
  public void test (TestHarness harness)
  {
    ZipEntry entry = new ZipEntry("liver");
    harness.check(entry.getCompressedSize(), -1,
		  "default compressed size is -1");

    entry.setCompressedSize(5);
    harness.check(entry.getCompressedSize(), 5, "get and set compressed size");

    boolean exception;
    try
      {
	entry.setCompressedSize(-1);
	exception = false;
      }
    catch (IllegalArgumentException _)
      {
	exception = true;
      }
    harness.check(!exception, "set compressed size to -1");

    try
      {
	entry.setCompressedSize(-7);
	exception = false;
      }
    catch (IllegalArgumentException _)
      {
	exception = true;
      }
    harness.check(!exception, "set compressed size to -7");
    harness.check(entry.getCompressedSize(), -7, "get compressed size as -7");

    long val = 5L + Integer.MAX_VALUE;
    try
      {
	entry.setCompressedSize(val);
	exception = false;
      }
    catch (IllegalArgumentException _)
      {
	exception = true;
      }
    harness.check(!exception, "set compressed size to long value");
    harness.check(entry.getCompressedSize(), val, "get long compressed size");
  }
}
