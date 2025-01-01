// Tags: JDK1.0

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Jeroen Frijters <jeroen@frijters.net>.

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
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.util.Date;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.util.Date;

public class range implements Testlet
{
  private TestHarness harness;

  public void test (TestHarness harness)
  {
    this.harness = harness;

    check(70, 0, 1, 0, 0, 0, 0);
    check(104, 9, 12, 0, 0, 0, 1097539200000L);
    check(104, 9, 12, 0, 0, 0, 1097539200000L);
    check(104, 9, 12, 12, 34, 0, 1097584440000L);
    check(104, 9, 12, 12, 34, 56, 1097584496000L);
    check(104, -1, 0, 0, 0, 0, 1070150400000L);
    check(104, 99, 99, 99, 99, 99, 1342068039000L);
    check(104, 999, 999, 999, 999, 999, 3789878139000L);
    check(104, -1, -1, -1, -1, -1, 1070060339000L);
    check(104, -999, -999, -999, -999, -999, -1644306939000L);
  }

  private void check(int year, int month, int day, int hours, int mins, int secs, long l)
  {
    try
      {
        Date d = new Date(year, month, day, hours, mins, secs);
        harness.check(d.getTime() - d.getTimezoneOffset() * 60 * 1000 == l);
      }
    catch (Throwable t)
      {
        harness.debug(t);
        harness.check(false);
      }
  }
}
