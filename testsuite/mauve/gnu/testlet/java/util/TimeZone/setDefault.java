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

// Tags: JDK1.1

package gnu.testlet.java.util.TimeZone;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.SimpleTimeZone;
import java.util.TimeZone;

public class setDefault implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.checkPoint("set/restore default TimeZone");
    
    SimpleTimeZone stz = new SimpleTimeZone(60 * 60 * 1000, "MyTZ");
    
    TimeZone old = TimeZone.getDefault();    
    TimeZone.setDefault(stz);
    
    harness.check(TimeZone.getDefault().getID(), stz.getID());

    TimeZone.setDefault(null);
    harness.check(TimeZone.getDefault().getID(), old.getID());
  }
}
