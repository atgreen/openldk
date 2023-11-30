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


package gnu.testlet.javax.swing.JFrame;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.GraphicsConfiguration;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.accessibility.AccessibleContext;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JRootPane;
import javax.swing.UIManager;

/**
 * Some checks for the constructors of the {@link JFrame} class.
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
    // Testing JFrame titles and invisibility
    testConstructor1(harness);
    testConstructor2(harness);
    testConstructor3(harness);
    testConstructor4(harness);
    testConstructor5(harness);
    testConstructor6(harness);
    // Testing default properties
    testFrameInit1(harness);
    // Testing if-clause
    testFrameInit2(harness);
  }

  /**
   * Constructor Test #1 No JFrame title is specified.
   */
  public void testConstructor1(TestHarness harness)
  {
    JFrame f = new JFrame();
    harness.check(f.getTitle(), "");
    harness.check(f.isShowing(), false);
  }

  /**
   * Constructor Test #2 JFrame title is specified, but not <code>null</code>.
   */
  public void testConstructor2(TestHarness harness)
  {
    JFrame f = new JFrame("JFrameName");
    harness.check(f.getTitle(), "JFrameName");
  }

  /**
   * Constructor Test #2 JFrame title is specified, but <code>null</code>.
   */
  public void testConstructor3(TestHarness harness)
  {
    JFrame f = new JFrame("");
    harness.check(f.getTitle(), "");
  }

  /**
   * Constructor Test #3 No JFrame title is specified and
   * {@link GraphicsConfiguration} specified.
   */
  public void testConstructor5(TestHarness harness)
  {
    JFrame f = new JFrame((GraphicsConfiguration) null);
    harness.check(f.getTitle(), "");
    harness.check(f.isShowing(), false);
  }

  /**
   * Constructor #4 JFrame title is specified and {@link GraphicsConfiguration}
   * specified, but <code>null</code>.
   */
  public void testConstructor4(TestHarness harness)
  {
    JFrame f = new JFrame("JFrameName", null);
    harness.check(f.getTitle(), "JFrameName");
  }

  /**
   * Constructor #4 No JFrame title is specified and
   * {@link GraphicsConfiguration} specified, but <code>null</code>.
   */
  public void testConstructor6(TestHarness harness)
  {
    JFrame f = new JFrame("", null);
    harness.check(f.getTitle(), "");
  }

  /**
   * Default JFrame Properties Test
   */
  public void testFrameInit1(TestHarness harness)
  {
    JFrame f = new JFrame();

    harness.check(f.getAccessibleContext() instanceof AccessibleContext);
    harness.check(f.getAccessibleContext() != null);

    harness.check(f.getBackground(), UIManager.getColor("control"));

    harness.check(f.getContentPane() instanceof Container);
    harness.check(f.getContentPane() != null);

    harness.check(f.getDefaultCloseOperation(), 1);

    harness.check(f.getGlassPane() instanceof Component);
    harness.check(f.getGlassPane() != null);

    harness.check(f.getLayeredPane() instanceof JLayeredPane);
    harness.check(f.getLayeredPane() != null);

    harness.check(f.getLayout() instanceof BorderLayout);
    harness.check(((BorderLayout) f.getLayout()).getHgap(), 0);
    harness.check(((BorderLayout) f.getLayout()).getVgap(), 0);

    harness.check(f.getJMenuBar(), null);

    harness.check(f.getRootPane() instanceof JRootPane);
    harness.check(f.getRootPane() != null);
  }

  /**
   * If-Clause Test
   */
  public void testFrameInit2(TestHarness harness)
  {
    JFrame.setDefaultLookAndFeelDecorated(true);
    JFrame f = new JFrame();
    harness.check(f.isUndecorated(), true);
    harness.check(f.getRootPane().getWindowDecorationStyle(), 1);
  }

}
