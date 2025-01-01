// Tags: JDK1.0

// Copyright (C) 2005 Free Software Foundation, Inc.

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
import java.util.TimeZone;

public class getTimezoneOffset implements Testlet
{
  TestHarness harness;
  
  public void test (TestHarness harness)
  {
    this.harness = harness;
    
    TimeZone.setDefault(TimeZone.getTimeZone("America/Toronto"));
    check(300, 240);
    TimeZone.setDefault(TimeZone.getTimeZone("Pacific/Auckland"));
    check(-780, -720);
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/London"));
    check(0, -60);
  }

  private void check(int offset_january, int offset_august)
  {
    harness.checkPoint("getTimezoneOffset for zone: " +
      TimeZone.getDefault().getID());
    Date d = new Date(96, 1, 14);
    System.out.println (d.getTimezoneOffset());
    harness.check (d.getTimezoneOffset(), offset_january);
    d = new Date(96, 8, 1);
    System.out.println (d.getTimezoneOffset());
    harness.check (d.getTimezoneOffset(), offset_august);
  }
}
