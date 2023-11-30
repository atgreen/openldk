// Tags: JDK1.4

// Copyright (C) 2006 Tania Bento <tbento2redhat.com>

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


package gnu.testlet.javax.swing.JButton;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonModel;
import javax.swing.ImageIcon;
import javax.swing.JButton;

/**
 * Some checks for the constructors of the {@link JButton} class.
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
    // Testing button text and icons
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
    // Testing default properties
    testProperties(harness);
  }

  /**
   * Constructor #1 No button text and icon specified.
   */
  public void testConstructor1(TestHarness harness)
  {
    JButton b = new JButton();
    harness.check(b.getText(), "");
    harness.check(b.getIcon(), null);
    harness.check(b.isFocusable(), true);
  }

  /**
   * Constructor #2 Action specified.
   */
  public void testConstructor2(TestHarness harness)
  {
    // Creating an <code>Action</code> with no properties.
    Action myAction = new AbstractAction()
    {
      public void actionPerformed(ActionEvent evt)
      {
        System.out.println("MyAction");
      }
    };
    JButton b = new JButton(myAction);
    harness.check(b.getAction(), myAction);
    harness.check(b.isFocusable(), true);
  }

  /**
   * Constructor #2 Action specified as <code>null</code>.
   */
  public void testConstructor3(TestHarness harness)
  {
    Action myAction = null;
    JButton b = new JButton(myAction);
    harness.check(b.getAction(), null);
  }

  /**
   * Constructor #3 Icon specified.
   */
  public void testConstructor4(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JButton b = new JButton(i);
    harness.check(b.getIcon(), i);
    harness.check(b.isFocusable(), true);
  }

  /**
   * Constructor #3 Icon specified as <code>/null<code>.
   */
  public void testConstructor5(TestHarness harness)
  {
    ImageIcon i = null;
    JButton b = new JButton(i);
    harness.check(b.getIcon(), null);
  }

  /**
   * Constructor #4 Button text specified.
   */
  public void testConstructor6(TestHarness harness)
  {
    JButton b = new JButton("Button Text");
    harness.check(b.getText(), "Button Text");
    harness.check(b.isFocusable(), true);
  }

  /**
   * Constructor #4 Button text specified as empty string.
   */
  public void testConstructor7(TestHarness harness)
  {
    JButton b = new JButton("");
    harness.check(b.getText(), "");
  }

  /**
   * Constructor #4 Button text specified as <code>/null<code>.
   */
  public void testConstructor8(TestHarness harness)
  {
    String buttonText = null;
    JButton b = new JButton(buttonText);
    harness.check(b.getText(), "");
  }

  /**
   * Constructor #5 Button text specified and icon specified.
   */
  public void testConstructor9(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JButton b = new JButton("Button Text", i);
    harness.check(b.getText(), "Button Text");
    harness.check(b.getIcon(), i);
    harness.check(b.isFocusable(), true);
  }

  /**
   * Constructor #5 Button text specified and icon specified as
   * <code>null</code>.
   */
  public void testConstructor10(TestHarness harness)
  {
    JButton b = new JButton("Button Text", null);
    harness.check(b.getText(), "Button Text");
    harness.check(b.getIcon(), null);
  }

  /**
   * Constructor #5 Button text specified as <code>null</code> and icon
   * specified.
   */
  public void testConstructor11(TestHarness harness)
  {
    ImageIcon i = new ImageIcon();
    JButton b = new JButton(null, i);
    harness.check(b.getText(), "");
    harness.check(b.getIcon(), i);
  }

  /**
   * Constructor #5 Button text specified as <code>null</code> and icon
   * specified as <code>null</code>.
   */
  public void testConstructor12(TestHarness harness)
  {
    JButton b = new JButton(null, null);
    harness.check(b.getText(), "");
    harness.check(b.getIcon(), null);
  }

  /**
   * Default JButton Properties Test
   */
  public void testProperties(TestHarness harness)
  {
    JButton b = new JButton();

    harness.check(b.getUIClassID(), "ButtonUI");
    harness.check(b.getModel() instanceof ButtonModel);
    harness.check(b.getModel() != null);
    
    harness.check(b.getActionCommand(), "");
    harness.check(b.isBorderPainted(), true);
    harness.check(b.isContentAreaFilled(), true);
    harness.check(b.getDisabledIcon(), null);
    harness.check(b.getDisabledSelectedIcon(), null);
    harness.check(b.isEnabled(), true);
    harness.check(b.isFocusPainted(), true);
    harness.check(b.getHorizontalAlignment(), 0);
    harness.check(b.getHorizontalTextPosition(), 11);
    harness.check(b.getIcon(), null);
    harness.check(b.getLabel(), "");
    harness.check(b.getMnemonic(), 0);
    harness.check(b.getMargin().top, 2);
    harness.check(b.getMargin().left, 14);
    harness.check(b.getMargin().bottom, 2);
    harness.check(b.getMargin().right, 14);
    harness.check(b.getPressedIcon(), null);
    harness.check(b.isRolloverEnabled(), true);
    harness.check(b.getRolloverIcon(), null);
    harness.check(b.getRolloverSelectedIcon(), null);
    harness.check(b.isSelected(), false);
    harness.check(b.getSelectedIcon(), null);
    harness.check(b.getSelectedObjects(), null);
    harness.check(b.getText(), "");
    harness.check(b.getVerticalAlignment(), 0);
    harness.check(b.getVerticalTextPosition(), 0);

    harness.check(b.getModel() instanceof ButtonModel);
    harness.check(b.getModel() != null);

    harness.check(b.isDefaultCapable(), true);
  }

}
