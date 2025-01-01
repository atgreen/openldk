// Tags: JDK1.2

// Copyright (C) 2005 David Gilbert <david.gilbert@object-refinery.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.awt.BorderLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Rectangle;

import javax.swing.JPanel;

/**
 * Some checks for the layoutContainer() method in the 
 * {@link BorderLayout} class.  
 */
public class layoutContainer implements Testlet 
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)   
  {
    test0(harness);
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
    test5(harness);
    test6(harness);
    test7(harness);
    test8(harness);
    test9(harness);
    test10(harness);
    test11(harness);
  }

  /**
   * All 5 components.
   */
  private void test0(TestHarness harness) 
  { 
    harness.checkPoint("test0");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(200, 200);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.CENTER);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(10, 54));
    p.add(p2, BorderLayout.NORTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(100, 74));
    p.add(p3, BorderLayout.WEST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(92, 33));
    p.add(p4, BorderLayout.EAST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(18, 64));
    p.add(p5, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(100, 54, 8, 82));
    harness.check(p2.getBounds(), new Rectangle(0, 0, 200, 54));
    harness.check(p3.getBounds(), new Rectangle(0, 54, 100, 82));
    harness.check(p4.getBounds(), new Rectangle(108, 54, 92, 82));
    harness.check(p5.getBounds(), new Rectangle(0, 136, 200, 64));
  }
  
  /**
   * Single component in the center.
   */
  private void test1(TestHarness harness) 
  { 
    harness.checkPoint("test1");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.CENTER);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 120));
  }

  /**
   * Single component, NORTH.
   */
  private void test2(TestHarness harness) 
  { 
    harness.checkPoint("test2");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 34));
  }

  /**
   * Single component, SOUTH.
   */
  private void test3(TestHarness harness) 
  { 
    harness.checkPoint("test3");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 86, 100, 34));
  }

  /**
   * Single component, EAST.
   */
  private void test4(TestHarness harness) 
  { 
    harness.checkPoint("test4");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.EAST);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(88, 0, 12, 120));
  }

  /**
   * Single component, WEST.
   */
  private void test5(TestHarness harness) 
  { 
    harness.checkPoint("test5");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 12, 120));
  }
  
  /**
   * Two components, NORTH and SOUTH.
   */
  private void test6(TestHarness harness) 
  { 
    harness.checkPoint("test6");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 34));
    harness.check(p2.getBounds(), new Rectangle(0, 111, 100, 9));
    
    // try overlapping
    p1.setPreferredSize(new Dimension(12, 66));
    p2.setPreferredSize(new Dimension(8, 77));
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 66));
    harness.check(p2.getBounds(), new Rectangle(0, 43, 100, 77));
    
  }
  
  /**
   * Two components, NORTH and EAST.
   */
  private void test7(TestHarness harness) 
  { 
    harness.checkPoint("test7");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.EAST);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 34));
    harness.check(p2.getBounds(), new Rectangle(92, 34, 8, 86));
  }
  
  /**
   * Two components, NORTH and WEST.
   */
  private void test8(TestHarness harness) 
  { 
    harness.checkPoint("test8");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.WEST);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 100, 34));
    harness.check(p2.getBounds(), new Rectangle(0, 34, 8, 86));
  }

  /**
   * Two components, EAST and WEST.
   */
  private void test9(TestHarness harness) 
  { 
    harness.checkPoint("test9");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.EAST);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 12, 120));
    harness.check(p2.getBounds(), new Rectangle(92, 0, 8, 120));

    // try overlapping
    p1.setPreferredSize(new Dimension(66, 10));
    p2.setPreferredSize(new Dimension(77, 12));
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 66, 120));
    harness.check(p2.getBounds(), new Rectangle(23, 0, 77, 120));
  }

  /**
   * Two components, EAST and SOUTH.
   */
  private void test10(TestHarness harness) 
  { 
    harness.checkPoint("test10");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.EAST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(88, 0, 12, 111));
    harness.check(p2.getBounds(), new Rectangle(0, 111, 100, 9));
  }

  /**
   * Two components, SOUTH and WEST.
   */
  private void test11(TestHarness harness) 
  { 
    harness.checkPoint("test11");
    JPanel p = new JPanel(new BorderLayout());
    p.setSize(100, 120);
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p1.getBounds(), new Rectangle(0, 0, 12, 111));
    harness.check(p2.getBounds(), new Rectangle(0, 111, 100, 9));
  }

}
