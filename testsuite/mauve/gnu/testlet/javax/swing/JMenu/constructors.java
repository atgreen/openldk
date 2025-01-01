/* constructors.java 
 Copyright (C) 2006 Red Hat, Tania Bento <tbento@redhat.com>
 
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

// Tags: 1.4


package gnu.testlet.javax.swing.JMenu;

import java.awt.Component;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.DefaultButtonModel;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;
import javax.swing.SwingConstants;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

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
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
  }

  /**
   * Constructor #1.
   */
  public void testConstructor1(TestHarness harness)
  {
    JMenu m = new JMenu();
    harness.check(m.isOpaque(), false);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
  }

  /**
   * Constructor #2. JMenu text specified.
   */
  public void testConstructor2(TestHarness harness)
  {
    JMenu m = new JMenu("Menu Text");
    harness.check(m.getText(), "Menu Text");
    harness.check(m.getIcon(), null);
    harness.check(m.getPopupMenu().getInvoker() instanceof JMenu);
    harness.check(m.isOpaque(), false);
  }

  /**
   * Constructor #2. JMenu text specified as <code>null</code>.
   */
  public void testConstructor3(TestHarness harness)
  {
    String title = null;
    JMenu m = new JMenu(title);
    harness.check(m.getText(), "");
  }

  /**
   * Constructor #3. Action specified.
   */
  public void testConstructor4(TestHarness harness)
  {
    // Creating an <code>Action</code> with no properties.
    Action myAction = new AbstractAction()
    {
      public void actionPerformed(ActionEvent evt)
      {
        System.out.println("MyAction");
      }
    };
    JMenu m = new JMenu(myAction);
    harness.check(m.getText(), null);
    harness.check(m.getIcon(), null);
    harness.check(m.getAction(), myAction);
    harness.check(m.getName(), null);
    harness.check(m.getAccelerator(), null);
    harness.check(m.getMnemonic(), 0);
    harness.check(m.getActionCommand(), null);
    harness.check(m.getPopupMenu().getInvoker() instanceof JMenu);
    harness.check(m.isOpaque(), false);
  }

  /**
   * Constructor #3. Action specified as <code>null</code>.
   */
  public void testConstructor5(TestHarness harness)
  {
    Action myAction = null;
    JMenu m = new JMenu(myAction);
    harness.check(m.getAction(), null);
  }

  /**
   * Constructor #4. JMenu text and boolean both specified. Note: This test does
   * not check any properties related to tearOff because it has not yet been
   * implemented, both in Sun and Classpath. It does however ensure that when
   * evoking calls to this unimplemented method, both behave in the same manner.
   */
  public void testConstructor6(TestHarness harness)
  {
    JMenu m = new JMenu("Menu Text", true);
    harness.check(m.getText(), "Menu Text");
    harness.check(m.getIcon(), null);
    boolean pass = false;
    try
      {
        m.isTearOff();
      }
    catch (Error e)
      {
        pass = true;
      }
    harness.check(pass);
  }

  /**
   * JMenu Properties.
   */
  public void testConstructor7(TestHarness harness)
  {
    JMenu m = new JMenu();
    // Properties set by AbstractButton
    harness.check(m.getVerticalAlignment(), SwingConstants.CENTER);
    harness.check(m.getVerticalTextPosition(), SwingConstants.CENTER);
    harness.check(m.isBorderPainted(), true);
    harness.check(m.isContentAreaFilled(), true);
    harness.check(m.isFocusPainted(), false);
    harness.check(m.isFocusable(), true);
    harness.check(m.getAlignmentX(), 0.5f);
    harness.check(m.getAlignmentY(), 0.5f);
    harness.check(m.getDisplayedMnemonicIndex(), - 1);
    // Properties set by JMenu's init method.
    harness.check(m.isFocusPainted(), false);
    harness.check(m.getHorizontalAlignment(), SwingConstants.LEADING);
    harness.check(m.getHorizontalTextPosition(), SwingConstants.TRAILING);
    // JMenu's default properties
    harness.check(m.getModel() instanceof DefaultButtonModel);
    harness.check(m.getModel() != null);
    harness.check(m.getUIClassID(), "MenuUI");
    harness.check(m.getComponent() instanceof Component);
    harness.check(m.getComponent() != null);
    harness.check(m.getDelay(), 200);
    harness.check(m.getItem(0), null);
    harness.check(m.getItemCount(), 0);
    harness.check(m.getLayout(), null);
    harness.check(m.getMenuComponent(0), null);
    harness.check(m.getMenuComponentCount(), 0);
    harness.check(m.getPopupMenu() instanceof JPopupMenu);
    harness.check(m.getPopupMenu() != null);
    harness.check(m.isPopupMenuVisible(), false);
    harness.check(m.isSelected(), false);
    harness.check(m.getSubElements() instanceof MenuElement[]);
    harness.check(m.getSubElements() != null);
    harness.check(m.isTopLevelMenu(), false);
  }

}
