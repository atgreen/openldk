// Tags: JDK1.2 

// Copyright (C) 2006 Roman Kennke (kennke@aicas.com)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version. 

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.javax.swing.plaf.basic.BasicOptionPaneUI.PropertyChangeHandler;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.plaf.basic.BasicOptionPaneUI;

public class propertyChange
  implements Testlet
{

  class TestOptionPaneUI
    extends BasicOptionPaneUI
  {
    protected void installComponents()
    {
      super.installComponents();
      // Uncomment to see that the call originates in propertyChange().
      //Thread.dumpStack();
      installComponentsCalled = true;
    }

    protected void uninstallComponents()
    {
      super.uninstallComponents();
      // Uncomment to see that the call originates in propertyChange().
      // Thread.dumpStack();
      uninstallComponentsCalled = true;
    }
  }

  boolean installComponentsCalled;
  boolean uninstallComponentsCalled;

  /**
   * The entry point.
   *
   * @param h the test harness
   */
  public void test(TestHarness h)
  {
    testVisualProperties(h);
  }

  /**
   * This tests if the BasicOptionPaneUI correctly uninstalls and reinstalls
   * the components on the JOptionPane. That is, it should call
   * uninstallComponents() and installComponents() when any of the
   * visual properties changes.
   *
   * @param h the test harness
   */
  private void testVisualProperties(TestHarness h)
  {
    JOptionPane p = new JOptionPane();
    TestOptionPaneUI ui = new TestOptionPaneUI();
    p.setUI(ui);

    installComponentsCalled = false;
    uninstallComponentsCalled = false;

    p.setIcon(new ImageIcon());
    checkReinstalled(h);

    p.setInitialSelectionValue("Hello World");
    checkReinstalled(h);

    p.setInitialValue(new Object());
    checkReinstalled(h);

    p.setMessage(new Object());
    checkReinstalled(h);

    p.setMessageType(JOptionPane.ERROR_MESSAGE);
    checkReinstalled(h);

    p.setOptions(new Object[0]);
    checkReinstalled(h);

    p.setOptionType(JOptionPane.NO_OPTION);
    checkReinstalled(h);

    p.setWantsInput(false);
    p.setWantsInput(true);
    checkReinstalled(h);
  }

  private void checkReinstalled(TestHarness h)
  {
    h.check(installComponentsCalled, true);
    h.check(uninstallComponentsCalled, true);
    installComponentsCalled = false;
    uninstallComponentsCalled = false;
  }
}