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


package gnu.testlet.javax.swing.JCheckBoxMenuItem;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonModel;
import javax.swing.DefaultButtonModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Some checks for the constructors of the {@link javax.swing.JCheckBoxMenuItem}
 * class.
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
    // Testing labels, icons, and states
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    testConstructor7(harness);
    testConstructor8(harness);
    testConstructor9(harness);
    testConstructor10(harness);
    testConstructor11(harness);
    testConstructor12(harness);
    testConstructor13(harness);
    // Testing the default properties
    testDefaultProperties(harness);
    // testConstructor15(harness);
  }

  /**
   * Constructor #1 Label and Icon not specified.
   */
  public void testConstructor1(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem();
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #2 Icon specified.
   */
  public void testConstructor2(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(i);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), i);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #2 Icon specified as <code>null</code>.
   */
  public void testConstructor3(TestHarness harness)
  {
    ImageIcon i = null;
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(i);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
  }

  /**
   * Constructor #3 Label specified.
   */
  public void testConstructor4(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem("Label");
    harness.check(m.getText(), "Label");
    harness.check(m.getIcon(), null);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #3 Label specified as <code>null</code>.
   */
  public void testConstructor5(TestHarness harness)
  {
    String label = null;
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(label);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
  }

  /**
   * Constructor #4 Action specified.
   */
  public void testConstructor6(TestHarness harness)
  {
    // Creating an <code>Action</code> with no properties.
    Action myAction = new AbstractAction()
    {
      public void actionPerformed(ActionEvent evt)
      {
        System.out.println("MyAction");
      }
    };
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(myAction);
    harness.check(m.getAction(), myAction);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #4 Action specified as <code>null</code>.
   */
  public void testConstructor7(TestHarness harness)
  {
    Action myAction = null;
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(myAction);
    harness.check(m.getAction(), null);
  }

  /**
   * Constructor #5 Label and Icon both specified.
   */
  public void testConstructor8(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JCheckBoxMenuItem m = new JCheckBoxMenuItem("Label", i);
    harness.check(m.getText(), "Label");
    harness.check(m.getIcon(), i);
    harness.check(m.getState(), false);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #5 Label and Icon specifed as <code>null</code>.
   */
  public void testConstructor9(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(null, null);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
    harness.check(m.getState(), false);
  }

  /**
   * Constructor #6 Label and State both specified.
   */
  public void testConstructor10(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem("Label", true);
    harness.check(m.getText(), "Label");
    harness.check(m.getIcon(), null);
    harness.check(m.getState(), true);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #6 Label specified as <code>null</code> and State specified
   * as <code>false</code>.
   */
  public void testConstructor11(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(null, false);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
    harness.check(m.getState(), false);
  }

  /**
   * Constructor #7 Label, Icon, and State specified.
   */
  public void testConstructor12(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JCheckBoxMenuItem m = new JCheckBoxMenuItem("Label", i, true);
    harness.check(m.getText(), "Label");
    harness.check(m.getIcon(), i);
    harness.check(m.getState(), true);
    harness.check(m.isFocusable(), false);
  }

  /**
   * Constructor #7 Label and Icon specified as <code>null</code> and State
   * specified as <code>false</code>.
   */
  public void testConstructor13(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem(null, null, false);
    harness.check(m.getText(), "");
    harness.check(m.getIcon(), null);
    harness.check(m.getState(), false);
  }

  /**
   * Testing default properties.
   */
  public void testDefaultProperties(TestHarness harness)
  {
    JCheckBoxMenuItem m = new JCheckBoxMenuItem("Label", new ImageIcon(), true);
    // Properties set by init method in JMenuItem.java.
    harness.check(m.isFocusPainted(), false);
    harness.check(m.getHorizontalAlignment(), JButton.LEADING);
    harness.check(m.getHorizontalTextPosition(), JButton.TRAILING); 
    // Additional JMenuITem.java properties
    harness.check(m.getUIClassID(), "CheckBoxMenuItemUI");
    harness.check(m.getAccelerator(), null);
    harness.check(m.isArmed(), false);
    harness.check(m.isEnabled(), true);
    harness.check(m.isRequestFocusEnabled(), true);
    // Properties set by AbstractButton.java's constructor.
    harness.check(m.getHorizontalAlignment(), 10);
    harness.check(m.getHorizontalTextPosition(), 11);
    harness.check(m.getVerticalAlignment(), 0);
    harness.check(m.getVerticalTextPosition(), 0);
    harness.check(m.isBorderPainted(), true);
    harness.check(m.isContentAreaFilled(), true);
    harness.check(m.isFocusPainted(), false);
    harness.check(m.isFocusable(), false);
    harness.check(m.getAlignmentX(), 0.5f);
    harness.check(m.getAlignmentY(), 0.5f);
    harness.check(m.getDisplayedMnemonicIndex(), - 1);
    harness.check(m.isOpaque(), true);
    //Additional AbstractButton.java properties
    harness.check(m.getActionCommand(), "Label");
    harness.check(m.getMnemonic(), 0);
    harness.check(m.getMargin().top, 2);
    harness.check(m.getMargin().left, 2);
    harness.check(m.getMargin().bottom, 2);
    harness.check(m.getMargin().right, 2);
    harness.check(m.getPressedIcon(), null);
    harness.check(m.isRolloverEnabled(), false);
    harness.check(m.getRolloverIcon(), null);
    harness.check(m.getRolloverSelectedIcon(), null);
    harness.check(m.isSelected(), true);
    harness.check(m.getSelectedIcon(), null);
    // Properties set by JCheckBoxMenuItem.java constructor itself.
    harness.check(m.getModel() instanceof DefaultButtonModel);
    harness.check(m.getModel() != null);
    harness.check(m.isVisible(), true);
  }

}
