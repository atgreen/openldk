/* constructor.java -- some checks for the constructor in the TabSet class.
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

// Tags: JDk1.4

package gnu.testlet.javax.swing.text.TabSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

public class constructor implements Testlet
{
  public void test(TestHarness harness)
  {
    TabStop ts1 = new TabStop(1.0f);
    TabStop ts2 = new TabStop(2.0f);
    TabStop ts3 = new TabStop(3.0f);
    TabStop[] tabs = new TabStop[] {ts1, ts2, ts3};
    TabSet s = new TabSet(tabs);
    harness.check(s.getTabCount(), 3);
    harness.check(s.getTab(0), ts1);
    harness.check(s.getTab(1), ts2);
    harness.check(s.getTab(2), ts3);
    
    // try modifying the original tab array
    TabStop ts4 = new TabStop(4.0f);
    tabs[1] = ts4;
    harness.check(s.getTab(1), ts2);
    
    // what if the original array is not ordered?
    TabStop[] tabs2 = new TabStop[] {ts1, ts3, ts2};
    TabSet s2 = new TabSet(tabs2);
    harness.check(s2.getTabCount(), 3);
    harness.check(s2.getTab(0), ts1);
    harness.check(s2.getTab(1), ts3);
    harness.check(s2.getTab(2), ts2);
    
    // try null
    s2 = new TabSet(null);
    harness.check(s2.getTabCount(), 0);
  }
}
