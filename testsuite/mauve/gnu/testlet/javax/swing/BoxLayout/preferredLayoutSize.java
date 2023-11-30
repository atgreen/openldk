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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.javax.swing.BoxLayout;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.AWTError;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSeparator;

/**
 * Some checks for the preferredLayoutSize() method defined in the 
 * {@link BoxLayout} class.
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
    testBasic(harness);
    testXAxis(harness);
    testYAxis(harness);
    testLineAxis(harness);
    testPageAxis(harness);
    testJSeparator(harness);
  }
  
  private void testBasic(TestHarness harness)
  {
    JPanel container = new JPanel();
    BoxLayout layout = new BoxLayout(container, BoxLayout.X_AXIS);
    harness.check(layout.preferredLayoutSize(container), new Dimension(0, 0));
    container.setBorder(BorderFactory.createEmptyBorder(1, 2, 3, 4));
    harness.check(layout.preferredLayoutSize(container), new Dimension(6, 4));
    
    // must call with original container
    boolean pass = false;
    try
      {
        /*Dimension result =*/ layout.preferredLayoutSize(new JPanel());
      }
    catch (AWTError e)
      {
        pass = true;
      }
    harness.check(pass);
  }
  
  private void testXAxis(TestHarness harness)
  {
    harness.checkPoint("testXAxis");
    JPanel container = new JPanel();
    container.setSize(100, 200);
    BoxLayout layout = new BoxLayout(container, BoxLayout.X_AXIS);
    container.setLayout(layout);
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 22));
    p2.setPreferredSize(new Dimension(33, 44));
    p3.setPreferredSize(new Dimension(55, 66));
    container.add(p1);
    container.add(p2);
    container.add(p3);
    harness.check(layout.preferredLayoutSize(container), new Dimension(99, 66));
  }
  
  private void testYAxis(TestHarness harness)
  {
    harness.checkPoint("testYAxis");
    JPanel container = new JPanel();
    container.setSize(100, 200);
    BoxLayout layout = new BoxLayout(container, BoxLayout.Y_AXIS);
    container.setLayout(layout);
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 22));
    p2.setPreferredSize(new Dimension(33, 44));
    p3.setPreferredSize(new Dimension(55, 66));
    container.add(p1);
    container.add(p2);
    container.add(p3);
    container.doLayout();
    harness.check(layout.preferredLayoutSize(container), new Dimension(55, 132));
  }

  private void testLineAxis(TestHarness harness)
  {
    harness.checkPoint("testLineAxis");
    JPanel container = new JPanel();
    container.setSize(100, 200);
    BoxLayout layout = new BoxLayout(container, BoxLayout.LINE_AXIS);
    container.setLayout(layout);
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 22));
    p2.setPreferredSize(new Dimension(33, 44));
    p3.setPreferredSize(new Dimension(55, 66));
    container.add(p1);
    container.add(p2);
    container.add(p3);
    container.doLayout();
    harness.check(layout.preferredLayoutSize(container), new Dimension(99, 66));
  }

  private void testPageAxis(TestHarness harness)
  {
    harness.checkPoint("testPageAxis");
    JPanel container = new JPanel();
    container.setSize(100, 200);
    BoxLayout layout = new BoxLayout(container, BoxLayout.PAGE_AXIS);
    container.setLayout(layout);
    JPanel p1 = new JPanel();
    JPanel p2 = new JPanel();
    JPanel p3 = new JPanel();
    p1.setPreferredSize(new Dimension(11, 22));
    p2.setPreferredSize(new Dimension(33, 44));
    p3.setPreferredSize(new Dimension(55, 66));
    container.add(p1);
    container.add(p2);
    container.add(p3);
    container.doLayout();
    harness.check(layout.preferredLayoutSize(container), new Dimension(55, 132));
  }

  private void testJSeparator(TestHarness harness)
  {
    harness.checkPoint("testJSeparator");
    JPanel container = new JPanel();
    container.setSize(100, 200);
    BoxLayout layout = new BoxLayout(container, BoxLayout.Y_AXIS);
    container.setLayout(layout);
    JSeparator s1 = new JSeparator();
    container.add(s1);
    container.doLayout();
    harness.check(layout.preferredLayoutSize(container), new Dimension(0, 2));
  }


}



