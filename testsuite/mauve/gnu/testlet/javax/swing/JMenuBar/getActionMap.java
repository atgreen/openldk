/* getActionMap.java -- some checks for the getActionMap() method in the
       JMenuBar class.
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

package gnu.testlet.javax.swing.JMenuBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.JMenuBar;

public class getActionMap implements Testlet
{
  public void test(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();
    ActionMap m = mb.getActionMap();
    harness.check(m.keys(), null);
    ActionMap mp = m.getParent();
    //Object[] keys = mp.keys();
    //for (int i = 0; i < keys.length; i++)
    //    System.out.println(keys[i]);
    harness.check(mp.get("takeFocus") instanceof Action);
    harness.check(mp.keys().length, 1);
  }
}
