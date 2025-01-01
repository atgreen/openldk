// Tags: JDK1.4

/* constructors.java -- tests for the constructor of the JMenuBar class
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

import java.awt.Insets;

import javax.swing.JMenuBar;
import javax.swing.SingleSelectionModel;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalMenuBarUI;

/**
 * Some tests for the constructors in the {@link JMenuBar} class.
 */
public class constructors
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    // the default (and only) constructor is pretty boring,
    // let's just test default values

    harness.checkPoint("JMenuBar(constructor)");

    // make sure we're using the MetalLookAndFeel
    try
      {
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();
      }

    JMenuBar mb = new JMenuBar();
    harness.check(mb.getComponentCount(), 0);
    harness.check(mb.getMenuCount(), 0);
    harness.check(mb.getMargin(), new Insets(0, 0, 0, 0));

    try
      {
        // This should throw an exception, not return null
        mb.getMenu(0);
        harness.check(false);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        harness.check(true);
      }

    harness.check(mb.getSelectionModel() instanceof SingleSelectionModel);
    harness.check(mb.getSubElements().length, 0);
    harness.check(mb.getUI() instanceof MetalMenuBarUI);

    harness.check(mb.isBorderPainted());
    harness.check(mb.isSelected() == false);
  }

}
