/* getComponentPopupMenu.java -- some checks for the getComponentPopupMenu()
       method in the JComponent class.
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

// Tags: 1.5

package gnu.testlet.javax.swing.JComponent;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

public class getComponentPopupMenu implements Testlet
{
  public void test(TestHarness harness)
  {
    JPanel p = new JPanel();
    JPopupMenu popup1 = new JPopupMenu();
    JPopupMenu popup2 = new JPopupMenu();
    JComponent c = new JButton("ABC");
    p.add(c);
    harness.check(c.getComponentPopupMenu(), null);
    harness.check(c.getInheritsPopupMenu(), false);
    c.setComponentPopupMenu(popup1);
    harness.check(c.getComponentPopupMenu(), popup1);
    p.setComponentPopupMenu(popup2);
    harness.check(c.getComponentPopupMenu(), popup1);
    c.setComponentPopupMenu(null);
    harness.check(c.getComponentPopupMenu(), null);
    c.setInheritsPopupMenu(true);
    harness.check(c.getComponentPopupMenu(), popup2);
    p.setComponentPopupMenu(null);
    harness.check(c.getComponentPopupMenu(), null);
  }
}
