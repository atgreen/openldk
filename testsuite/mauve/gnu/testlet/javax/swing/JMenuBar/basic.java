// Tags: JDK1.4

/* basic.java -- some checks for simple methods in the JMenuBar class
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

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.plaf.basic.BasicMenuBarUI;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.plaf.metal.MetalMenuBarUI;
import javax.swing.plaf.multi.MultiMenuBarUI;

/**
 * Some checks for simple methods in the {@link JMenuBar} class.
 */
public class basic
    implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)
  {
    setMargin(harness);

    setUI(harness);

    setBorderPainted(harness);

    setSelected(harness);

  }

  public void setMargin(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();

    mb.setMargin(new Insets(5, 5, 5, 5));
    harness.check(mb.getMargin(), new Insets(5, 5, 5, 5));

    mb.setMargin(null);
    harness.check(mb.getMargin(), new Insets(0, 0, 0, 0));
  }

  public void setUI(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();

    // make sure we're using the MetalLookAndFeel
    try
      {
        UIManager.setLookAndFeel(new MetalLookAndFeel());
      }
    catch (UnsupportedLookAndFeelException e)
      {
        e.printStackTrace();
      }

    mb.setUI(new BasicMenuBarUI());
    harness.check(mb.getUI() instanceof BasicMenuBarUI);

    mb.setUI(new MultiMenuBarUI());
    harness.check(mb.getUI() instanceof MultiMenuBarUI);

    mb.updateUI();
    harness.check(mb.getUI() instanceof MetalMenuBarUI);
  }

  public void setBorderPainted(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();

    mb.setBorderPainted(false);
    harness.check(mb.isBorderPainted() == false);

    mb.setBorderPainted(true);
    harness.check(mb.isBorderPainted());
  }

  public void setSelected(TestHarness harness)
  {
    JMenuBar mb = new JMenuBar();

    harness.check(mb.isSelected() == false);

    JMenu menu = new JMenu("menu");
    mb.add(menu);
    mb.setSelected(menu);
    harness.check(mb.isSelected());
  }

}
