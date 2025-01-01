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

import javax.swing.JPanel;

/**
 * Some checks for the preferredLayoutSize() method in the 
 * {@link BorderLayout} class.  
 */
public class preferredLayoutSize implements Testlet 
{

  /**
   * Runs the test using the specified harness. 
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test(TestHarness harness)    
  {
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
    test12(harness);
    test13(harness);
    test14(harness);
    test15(harness);
    test16(harness);
    test17(harness);
  }

  /**
   * Single component in the center.
   */
  private void test1(TestHarness harness) 
  { 
    harness.checkPoint("test1");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.CENTER);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 34));
  }

  /**
   * Single component, NORTH.  
   */
  private void test2(TestHarness harness) 
  { 
    harness.checkPoint("test2");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 34));
  }

  /**
   * Single component, SOUTH.
   */
  private void test3(TestHarness harness) 
  { 
    harness.checkPoint("test3");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 34));
  }

  /**
   * Single component, EAST.
   */
  private void test4(TestHarness harness) 
  { 
    harness.checkPoint("test4");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.EAST);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 34));
  }

  /**
   * Single component, WEST.
   */
  private void test5(TestHarness harness) 
  { 
    harness.checkPoint("test5");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 34)) ;
  }

  /**
   * Two components, NORTH and SOUTH.
   */
  private void test6(TestHarness harness) 
  { 
    harness.checkPoint("test6");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 43));
  }

  /**
   * Two components, NORTH and EAST.
   */
  private void test7(TestHarness harness) 
  { 
    harness.checkPoint("test7");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.EAST);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 43));
  }

  /**
   * Two components, NORTH and WEST.
   */
  private void test8(TestHarness harness) 
  {    
    harness.checkPoint("test8");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.WEST);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 43));
  }

  /**
   * Two components, EAST and WEST.
   */
  private void test9(TestHarness harness) 
  {   
    harness.checkPoint("test9");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.EAST);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(20, 34));
  }

  /**
   * Two components, EAST and SOUTH.  
   */
  private void test10(TestHarness harness) 
  { 
    harness.checkPoint("test10");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.EAST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 43));
  }

  /**
   * Two components, SOUTH and WEST.
   */
  private void test11(TestHarness harness) 
  { 
    harness.checkPoint("test11");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(12, 34));
    p.add(p1, BorderLayout.WEST);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(8, 9));
    p.add(p2, BorderLayout.SOUTH);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 43));
  }

  /**
   * 5 components, NORTH the widest.
   */
  private void test12(TestHarness harness) 
  { 
    harness.checkPoint("test12");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 1));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(10, 2));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(3, 3));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(3, 4));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(3, 5));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(11, 8));
  }

  /**
   * 5 components, WEST+CENTER+EAST the widest.
   */
  private void test13(TestHarness harness) 
  { 
    harness.checkPoint("test13");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 1));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(10, 2));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(4, 3));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(4, 4));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(4, 5));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 8));
  }

  /**
   * 5 components, SOUTH the widest.
   */
  private void test14(TestHarness harness) 
  { 
    harness.checkPoint("test14");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(10, 1));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(11, 2));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(3, 3));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(3, 4));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(3, 5));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(11, 8));
  }

  /**
   * 5 components, NORTH+WEST+SOUTH the tallest.
   */
  private void test15(TestHarness harness) 
  { 
    harness.checkPoint("test15");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(1, 3));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(2, 3));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(3, 3));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(4, 4));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(5, 3));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 10));
  }

  /**
   * 5 components, NORTH+CENTER+SOUTH the tallest.
   */
  private void test16(TestHarness harness) 
  { 
    harness.checkPoint("test16");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(1, 3));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(2, 3));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(3, 3));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(4, 3));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(5, 4));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 10));
  }

  /**
   * 5 components, NORTH+EAST+SOUTH the tallest.
   */
  private void test17(TestHarness harness) 
  { 
    harness.checkPoint("test17");
    JPanel p = new JPanel(new BorderLayout());
    JPanel p1 = new JPanel();
    p1.setPreferredSize(new Dimension(1, 3));
    p.add(p1, BorderLayout.NORTH);
    JPanel p2 = new JPanel();
    p2.setPreferredSize(new Dimension(2, 3));
    p.add(p2, BorderLayout.SOUTH);
    JPanel p3 = new JPanel();
    p3.setPreferredSize(new Dimension(3, 4));
    p.add(p3, BorderLayout.EAST);
    JPanel p4 = new JPanel();
    p4.setPreferredSize(new Dimension(4, 3));
    p.add(p4, BorderLayout.WEST);
    JPanel p5 = new JPanel();
    p5.setPreferredSize(new Dimension(5, 3));
    p.add(p5);
    p.doLayout();
    harness.check(p.getPreferredSize(), new Dimension(12, 10));
  }

}
