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

public class setComment implements Testlet
{
  public void test (TestHarness harness)
  {
    boolean jdk7 = conformToJDK17();
    ZipEntry entry = new ZipEntry("test");
    harness.check(entry.getComment(), null, "default comment is null");

    entry.setComment("abc");
    harness.check(entry.getComment(), "abc", "get and set normal comment");

    boolean exception;
    try
      {
	entry.setComment(new String (new char [0xFFFF + 1]));
	if (jdk7) {
	  // should be 0xFFFF in case of JDK7
	  exception = entry.getComment().length() != 0xFFFF;
	}
	else
	{
	  exception = false;
	}
      }
    catch (IllegalArgumentException _)
      {
	// JDK6 should throw this exception, but JDK7 can not
	exception = !jdk7;
      }
    harness.check(exception, "comment larger then 65535 chars");

    entry.setComment(null);
    harness.check(entry.getComment(), null, "get and set null comment");
  }

  /**
    * Returns true if tested JRE conformns to JDK 1.7.
    * @author: Mark Wielaard
    */
  private static boolean conformToJDK17()
  {
    String[] javaVersion = System.getProperty("java.version").split("\\.");
    String vendorID = System.getProperty("java.vendor");
    // test of OpenJDK
    if ("Sun Microsystems Inc.".equals(vendorID))
      {
        return Long.parseLong(javaVersion[1]) >= 7;
      }
    return true;
  }
}
