/* setSelected.java -- some checks for the setSelected() method in the
       ButtonGroup class.
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

package gnu.testlet.javax.swing.ButtonGroup;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ButtonGroup;
import javax.swing.ButtonModel;
import javax.swing.JToggleButton;

public class setSelected implements Testlet
{
  public void test(TestHarness harness)
  {
    
    ButtonGroup g1 = new ButtonGroup();
    JToggleButton b1 = new JToggleButton("B1");
    ButtonModel m1 = b1.getModel();
    g1.add(b1);
    harness.check(m1.isSelected(), false);
    g1.setSelected(m1, false);
    harness.check(b1.isSelected(), false);
    harness.check(g1.isSelected(m1), false);
    harness.check(g1.getSelection(), null);
    
    ButtonGroup g2 = new ButtonGroup();
    JToggleButton b2 = new JToggleButton("B2");
    ButtonModel m2 = b2.getModel();
    g2.add(b2);
    harness.check(m2.isSelected(), false);
    g2.setSelected(m2, true);
    harness.check(b2.isSelected(), true);
    harness.check(g2.isSelected(m2), true);
    harness.check(g2.getSelection(), m2);

    ButtonGroup g3 = new ButtonGroup();
    JToggleButton b3 = new JToggleButton("B3");
    b3.setSelected(true);
    ButtonModel m3 = b3.getModel();
    g3.add(b3);
    harness.check(m3.isSelected(), true);
    g3.setSelected(m3, false);
    harness.check(b3.isSelected(), true);
    harness.check(g3.isSelected(m3), true);
    harness.check(g3.getSelection(), m3);

    ButtonGroup g4 = new ButtonGroup();
    JToggleButton b4 = new JToggleButton("B4");
    b4.setSelected(true);
    ButtonModel m4 = b4.getModel();
    g4.add(b4);
    harness.check(m4.isSelected(), true);
    g4.setSelected(m4, false);
    harness.check(b4.isSelected(), true);
    harness.check(g4.isSelected(m4), true);
    harness.check(g4.getSelection(), m4);

    // now work with a second button...
    JToggleButton b5 = new JToggleButton("B5");
    ButtonModel m5 = b5.getModel();
    g4.add(b5);
    harness.check(g4.isSelected(m5), false);
    // the following does nothing, because 'false' says we don't change the
    // state of the incoming model...
    g4.setSelected(m5, false);
    harness.check(b4.isSelected(), true);
    harness.check(g4.isSelected(m4), true);
    harness.check(g4.getSelection(), m4);
    
    // whereas this changes the selection...
    g4.setSelected(m5, true);
    harness.check(b4.isSelected(), false);
    harness.check(b5.isSelected(), true);
    harness.check(g4.isSelected(m4), false);
    harness.check(g4.isSelected(m5), true);
    harness.check(g4.getSelection(), m5);
    
  }
}
