// Tags: JDK1.4
// Uses: getBytes13

// Copyright (C) 2003 Free Software Foundation, Inc.

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

package gnu.testlet.java.lang.String;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class getBytes14 extends getBytes13 implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.checkPoint("getBytes14");

    // New canonical names in 1.4
    test1Encoding (harness, "US-ASCII",     "abc", ABC1);
    test1Encoding (harness, "windows-1252", "abc", ABC1);
    test1Encoding (harness, "ISO-8859-1",   "abc", ABC1);
    test1Encoding (harness, "ISO-8859-15",  "abc", ABC1);
    test1Encoding (harness, "ISO8859_15",   "abc", ABC1);
    test1Encoding (harness, "UTF-8",        "abc", ABC1);
    test1Encoding (harness, "UTF-16BE",     "abc", ABC3);
    test1Encoding (harness, "UTF-16LE",     "abc", ABC5);
  }
}
