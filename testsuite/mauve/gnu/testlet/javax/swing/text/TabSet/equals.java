/* equals.java -- some checks for the equals() method in the TabSet class.
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

// Tags: JDK1.5

package gnu.testlet.javax.swing.text.TabSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

public class equals implements Testlet
{
  public void test(TestHarness harness)
  {
    TabSet tsA = new TabSet(null);
    TabSet tsB = new TabSet(null);
    harness.check(tsA.equals(tsB));
    
    tsA = new TabSet(new TabStop[] {});
    tsB = new TabSet(new TabStop[] {});
    harness.check(tsA.equals(tsB));
    
    tsA = new TabSet(new TabStop[] {new TabStop(1.0f)});
    tsB = new TabSet(new TabStop[] {new TabStop(1.0f)});
    harness.check(tsA.equals(tsB));

    tsA = new TabSet(new TabStop[] {new TabStop(1.1f, TabStop.ALIGN_CENTER, 
            TabStop.LEAD_DOTS)});
    harness.check(!tsA.equals(tsB));
    tsB = new TabSet(new TabStop[] {new TabStop(1.1f, TabStop.ALIGN_CENTER, 
            TabStop.LEAD_DOTS)});
    harness.check(tsA.equals(tsB));
  
    harness.check(!tsA.equals(null));
    harness.check(!tsA.equals("a string"));
  }
}
