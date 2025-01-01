/* minimumLayoutSize.java -- Checks the minimumLayoutSize() method
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
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

// Tags: JDK1.2

package gnu.testlet.javax.swing.ScrollPaneLayout;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Insets;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests the minimumLayoutSize() method in ScrollPaneLayout under different
 * settings. It turns out that the minimumSize is determined using the
 * scrollpane's insets and the minimumSizes of the horizontal and vertical
 * scrollbars, except when the (h/v) scrollbar policy is set to NEVER.
 *  
 * @author Roman Kennke (kennke@aicas.com)
 */
public class minimumLayoutSize implements Testlet
{

  /**
   * The entry point for the tests.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testHorizontalScrollbarAlways(harness);
    testHorizontalScrollbarAsNeeded(harness);
    testHorizontalScrollbarNever(harness);
    testVerticalScrollbarAlways(harness);
    testVerticalScrollbarAsNeeded(harness);
    testVerticalScrollbarNever(harness);
  }

  /**
   * Tests with HORIZONTAL_SCROLLBAR_ALWAYS.
   *
   * @param h the test harness to use
   */
  private void testHorizontalScrollbarAlways(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    sp.getViewport().add(view);
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height
                          + c[2].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height
                          + c[2].getMinimumSize().height));
  }

  /**
   * Tests with HORIZONTAL_SCROLLBAR_AS_NEEDED.
   *
   * @param h the test harness to use
   */
  private void testHorizontalScrollbarAsNeeded(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    sp.getViewport().add(view);
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height
                          + c[2].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height
                          + c[2].getMinimumSize().height));
  }

  /**
   * Tests with HORIZONTAL_SCROLLBAR_NEVER.
   *
   * @param h the test harness to use
   */
  private void testHorizontalScrollbarNever(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    sp.getViewport().add(view);
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
  }

  /**
   * Tests with VERTICAL_SCROLLBAR_ALWAYS.
   *
   * @param h the test harness to use
   */
  private void testVerticalScrollbarAlways(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    sp.getViewport().add(view);
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width
                          + c[1].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width
                          + c[1].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
  }

  /**
   * Tests with VERTICAL_SCROLLBAR_AS_NEEDED.
   *
   * @param h the test harness to use
   */
  private void testVerticalScrollbarAsNeeded(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    sp.getViewport().add(view);
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width
                          + c[1].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width
                          + c[1].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
  }

  /**
   * Tests with VERTICAL_SCROLLBAR_NEVER.
   *
   * @param h the test harness to use
   */
  private void testVerticalScrollbarNever(TestHarness h)
  {
    JScrollPane sp = new JScrollPane();
    ScrollPaneLayout l = (ScrollPaneLayout) sp.getLayout();
    sp.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    sp.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    JPanel view = new JPanel();
    sp.getViewport().add(view);
    view.setMinimumSize(new Dimension(100, 100));
    Insets i = sp.getInsets();
    Component[] c = sp.getComponents();
    // Check ScrollPane size < view size.
    sp.setSize(50, 50);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
    // Check ScrollPane size > view size.
    sp.setSize(150, 150);
    h.check(l.minimumLayoutSize(sp),
            new Dimension(i.left + i.right + c[0].getMinimumSize().width,
                          i.top + i.bottom + c[0].getMinimumSize().height));
  }
}
