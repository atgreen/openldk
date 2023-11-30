/* constructors.java -- some checks for the constructors in the TabStop class.
   Copyright (C) 2006 David Gilbert <david.gilbert@object-refinery.com>
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

package gnu.testlet.javax.swing.text.TabStop;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.TabStop;

public class constructors implements Testlet
{
  public void test(TestHarness harness)
  {
    testConstructor1(harness);
    testConstructor2(harness);
  }
  
  public void testConstructor1(TestHarness harness)
  {
    harness.checkPoint("(float)");
    TabStop ts1 = new TabStop(1.0f);
    harness.check(ts1.getPosition(), 1.0f);
    harness.check(ts1.getAlignment(), TabStop.ALIGN_LEFT);
    harness.check(ts1.getLeader(), TabStop.LEAD_NONE);
    
    // try negative 
    ts1 = new TabStop(-1.0f);
    harness.check(ts1.getPosition(), -1.0f);
  }

  public void testConstructor2(TestHarness harness)
  {
    harness.checkPoint("(float, int, int)");
    TabStop ts1 = new TabStop(1.0f, TabStop.ALIGN_DECIMAL, TabStop.LEAD_DOTS);
    harness.check(ts1.getPosition(), 1.0f);
    harness.check(ts1.getAlignment(), TabStop.ALIGN_DECIMAL);
    harness.check(ts1.getLeader(), TabStop.LEAD_DOTS);
    
    // try bad alignment 
    ts1 = new TabStop(1.0f, 99, TabStop.LEAD_DOTS);
    harness.check(ts1.getPosition(), 1.0f);
    harness.check(ts1.getAlignment(), 99);
    harness.check(ts1.getLeader(), TabStop.LEAD_DOTS);

    // try bad lead 
    ts1 = new TabStop(1.0f, TabStop.ALIGN_DECIMAL, 99);
    harness.check(ts1.getPosition(), 1.0f);
    harness.check(ts1.getAlignment(), TabStop.ALIGN_DECIMAL);
    harness.check(ts1.getLeader(), 99);
  }

}
