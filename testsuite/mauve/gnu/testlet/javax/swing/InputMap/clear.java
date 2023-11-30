/* clear.java -- some checks for the clear() method in the InputMap class.
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

package gnu.testlet.javax.swing.InputMap;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.InputMap;
import javax.swing.KeyStroke;

public class clear implements Testlet
{
  public void test(TestHarness harness)
  {
    InputMap m = new InputMap();
    m.clear();
    harness.check(m.size(), 0);
    m.put(KeyStroke.getKeyStroke('a'), "AAA");
    harness.check(m.size(), 1);
    m.clear();
    harness.check(m.size(), 0);
    
    // confirm that this method doesn't change the parent
    InputMap p = new InputMap();
    p.put(KeyStroke.getKeyStroke('z'), "ZZZ");
    harness.check(p.size(), 1);
    m.setParent(p);
    m.clear();
    harness.check(p.size(), 1);
  }
}
