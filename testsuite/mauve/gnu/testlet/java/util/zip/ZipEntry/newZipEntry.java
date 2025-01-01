// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation
// Contributed by Mark Wielaard (mark@klomp.org)

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

public class newZipEntry implements Testlet
{
  public void test (TestHarness harness)
  {
    boolean exception;
    try
      {
	new ZipEntry(new String (new char [0xFFFF + 1]));
	exception = false;
      }
    catch (IllegalArgumentException _)
      {
	exception = true;
      }

    harness.check(exception, "name larger then 65535 chars");

    try
      {
	new ZipEntry((String)null);
	exception = false;
      }
    catch (NullPointerException _)
      {
	exception = true;
      }

    harness.check(exception, "name is null");
  }
}
