/* getActionMap.java -- some checks for the getActionMap() method in the
       JSplitPane class.
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

// Tags: JDK1.3

package gnu.testlet.javax.swing.JSplitPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ActionMap;
import javax.swing.JSplitPane;

public class getActionMap implements Testlet
{
  public void test(TestHarness harness)
  {
    JSplitPane sp = new JSplitPane();
    ActionMap m = sp.getActionMap();
    harness.check(m.keys(), null);
    ActionMap mp = m.getParent();
    harness.check(mp.get("negativeIncrement") != null);
    harness.check(mp.get("positiveIncrement") != null);
    harness.check(mp.get("selectMin") != null);
    harness.check(mp.get("selectMax") != null);
    harness.check(mp.get("startResize") != null);
    harness.check(mp.get("toggleFocus") != null);
    harness.check(mp.get("focusOutForward") != null);
    harness.check(mp.get("focusOutBackward") != null);
  }
}
