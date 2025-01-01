/* allKeys.java -- some checks for the allKeys() method in the InputMap class.
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

public class allKeys implements Testlet
{
  public void test(TestHarness harness)
  {
    InputMap m = new InputMap();
    harness.check(m.allKeys(), null);
    KeyStroke ks1 = KeyStroke.getKeyStroke('a');
    m.put(ks1, "AAA");
    KeyStroke[] keys = m.allKeys();
    harness.check(keys.length, 1);
    harness.check(keys[0], ks1);
    
    InputMap p = new InputMap();
    m.setParent(p);
    keys = m.allKeys();
    harness.check(keys.length, 1);
    harness.check(keys[0], ks1);
    
    KeyStroke ks2 = KeyStroke.getKeyStroke('b');
    p.put(ks2, "BBB");
    keys = m.allKeys();
    harness.check(keys.length, 2);
    harness.check(keys[0], ks2);
    harness.check(keys[1], ks1);
    
    // try a KeyStroke that is defined in both maps
    KeyStroke ks3 = KeyStroke.getKeyStroke('z');
    p.put(ks3, "ZZZ");
    m.put(ks3, "XXX");
    keys = m.allKeys();
    harness.check(keys.length, 3);
    harness.check(keys[0], ks2);
    harness.check(keys[1], ks3);
    harness.check(keys[2], ks1);
  }
}
