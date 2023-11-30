// Tags: JDK1.4

/* getComponentIndex.java -- tests for the getComponentIndex() method
       in the JMenuBar class
   Copyright (C) 2006 Francis Kung <fkung@redhat.com>
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


package gnu.testlet.javax.swing.JMenuBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.MenuElement;

/**
 * Some checks for the getMenu() method of the {@link JMenuBar} class.
 */
public class getComponentIndex
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();

    JMenu menu1 = new JMenu("menu1");
    mb.add(menu1);

    JMenu menu2 = new JMenu("menu2");
    mb.add(menu2);

    JLabel label = new JLabel("label");
    mb.add(label);

    JMenu menu3 = new JMenu("menu3");
    mb.add(menu3);

    harness.check(mb.getComponentIndex(menu1), 0);
    harness.check(mb.getComponentIndex(menu2), 1);
    harness.check(mb.getComponentIndex(label), 2);
    harness.check(mb.getComponentIndex(menu3), 3);
  }

}
