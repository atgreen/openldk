/* getTabIndexAfter.java -- some checks for the getTabIndexAfter() method in
       the TabSet class.
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

package gnu.testlet.javax.swing.text.TabSet;

import javax.swing.text.TabSet;
import javax.swing.text.TabStop;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getTabIndexAfter implements Testlet
{
  public void test(TestHarness harness)
  {
    TabStop ts1 = new TabStop(1.0f);
    TabStop ts2 = new TabStop(2.0f);
    TabStop ts3 = new TabStop(3.0f);
    TabStop[] tabs = new TabStop[] {ts1, ts2, ts3};
    TabSet s = new TabSet(tabs);
    harness.check(s.getTabIndexAfter(-1.0f), 0);
    harness.check(s.getTabIndexAfter(0.0f), 0);
    harness.check(s.getTabIndexAfter(0.5f), 0);
    harness.check(s.getTabIndexAfter(1.0f), 0);
    harness.check(s.getTabIndexAfter(1.5f), 1);
    harness.check(s.getTabIndexAfter(2.0f), 1);
    harness.check(s.getTabIndexAfter(2.5f), 2);
    harness.check(s.getTabIndexAfter(3.0f), 2);
    harness.check(s.getTabIndexAfter(3.5f), -1);
  }
}
