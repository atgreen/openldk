/* get.java -- some checks for the get() method in the InputMap class.
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

public class get implements Testlet 
{
  public void test(TestHarness harness)
  {
    InputMap m = new InputMap();
    KeyStroke ks1 = KeyStroke.getKeyStroke('a');
    harness.check(m.get(ks1), null);
    m.put(ks1, "ABC");
    harness.check(m.get(ks1), "ABC");
    
    harness.check(m.get(null), null);
    
    // check that a binding in the parent is found
    InputMap p = new InputMap();
    KeyStroke ks2 = KeyStroke.getKeyStroke('b');
    p.put(ks2, "XYZ");
    m.setParent(p);
    harness.check(m.get(ks2), "XYZ");
  }
}
