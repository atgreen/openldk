/* getActionMap.java -- some checks for the getActionMap() method in the
       JLabel class.
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

package gnu.testlet.javax.swing.JLabel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ActionMap;
import javax.swing.JButton;
import javax.swing.JLabel;

public class getActionMap implements Testlet
{
  public void test(TestHarness harness)
  {
    // this is the original version of the test - it shows no actions for
    // a regular label...
    JLabel label = new JLabel("XYZ");
    ActionMap m = label.getActionMap();
    harness.check(m.keys(), null);
    ActionMap mp = m.getParent();
    harness.check(mp, null);
    
    // but then I remembered that when a label has a mnemonic and target 
    // component, you can get the focus for the target component via the 
    // keyboard
    JLabel label2 = new JLabel("ABC");
    JButton button = new JButton("Target");
    label2.setLabelFor(button);
    label2.setDisplayedMnemonic('A');
    
    m = label2.getActionMap();
    harness.check(m.keys(), null);
    mp = m.getParent();
    harness.check(mp.size(), 2);
    harness.check(mp.get("press") != null);
    harness.check(mp.get("release") != null);
  }
}
