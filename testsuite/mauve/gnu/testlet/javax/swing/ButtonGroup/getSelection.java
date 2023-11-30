/* getSelection.java -- some checks for the getSelection() method in the
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
import javax.swing.JToggleButton;

public class getSelection implements Testlet
{
  public void test(TestHarness harness)
  {
    ButtonGroup g = new ButtonGroup();
    harness.check(g.getSelection(), null);
    JToggleButton b1 = new JToggleButton("B1");
    g.add(b1);
    harness.check(g.getSelection(), null);    
    JToggleButton b2 = new JToggleButton("B2");
    b2.setSelected(true);
    g.add(b2);
    harness.check(g.getSelection(), b2.getModel());    
  }
}
