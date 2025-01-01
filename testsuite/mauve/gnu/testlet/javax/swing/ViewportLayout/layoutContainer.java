// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.javax.swing.ViewportLayout;

import java.awt.Dimension;
import java.awt.Point;

import javax.swing.JPanel;
import javax.swing.JViewport;
import javax.swing.ViewportLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks the functionality of the LayoutManager javax.swing.ViewportLayout.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class layoutContainer implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
    testMinimumViewSize(harness);
  }

  /**
   * A simple test for the layout manager.
   * 
   * @param h the test harness to use
   */
  private void test1(TestHarness h)
  {
    JViewport vp = new JViewport();
    ViewportLayout layout = (ViewportLayout) vp.getLayout();
    JPanel view = new JPanel(){};
    vp.setView(view);
    view.setMinimumSize(new Dimension(100, 100));
    view.setPreferredSize(new Dimension(200, 200));
    view.setMaximumSize(new Dimension(300, 300));
    vp.setSize(400, 400);
    layout.layoutContainer(vp);
    h.check(view.getSize(), new Dimension(400, 400));
  }

  /**
   * Another simple test for the layout manager.
   * 
   * @param h the test harness to use
   */
  private void test2(TestHarness h)
  {
    JViewport vp = new JViewport();
    ViewportLayout layout = (ViewportLayout) vp.getLayout();
    JPanel view = new JPanel(){};
    vp.setView(view);
    view.setMinimumSize(new Dimension(100, 100));
    view.setPreferredSize(new Dimension(200, 200));
    view.setMaximumSize(new Dimension(300, 300));
    vp.setSize(150, 150);
    layout.layoutContainer(vp);
    h.check(view.getSize(), new Dimension(200, 200));
  }

  /**
   * Another simple test for the layout manager.
   * 
   * @param h the test harness to use
   */
  private void test3(TestHarness h)
  {
    JViewport vp = new JViewport();
    ViewportLayout layout = (ViewportLayout) vp.getLayout();
    JPanel view = new JPanel(){};
    vp.setView(view);
    view.setMinimumSize(new Dimension(100, 100));
    view.setPreferredSize(new Dimension(10, 10));
    view.setMaximumSize(new Dimension(300, 300));
    vp.setSize(50, 50);
    layout.layoutContainer(vp);
    h.check(view.getSize(), new Dimension(50, 50));
  }

  /**
   * There was a strange rule in Classpath's implementation that said that if
   * the viewport is larger than the view's minimumSize, the view's location
   * must be set to (0,0). This test proves that this is wrong.
   *
   * @param h the test harness to use
   */
  private void testMinimumViewSize(TestHarness h)
  {
    JViewport vp = new JViewport();
    ViewportLayout l = (ViewportLayout) vp.getLayout();
    JPanel view = new JPanel();
    view.setMinimumSize(new Dimension(100, 100));
    view.setPreferredSize(new Dimension(200, 200));
    view.setMaximumSize(new Dimension(300, 300));
    vp.setSize(150, 150);
    vp.setView(view);
    view.setBounds(50, 50, 100, 100);
    l.layoutContainer(vp);
    h.check(view.getLocation(), new Point(50, 50));
  }
}
