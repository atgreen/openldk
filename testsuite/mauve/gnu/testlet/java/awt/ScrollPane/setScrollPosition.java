/* setScrollPosition.java
   Copyright (C) 2006 
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

// Tags: JDK1.4

package gnu.testlet.java.awt.ScrollPane;

import java.awt.Button;
import java.awt.Point;
import java.awt.ScrollPane;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setScrollPosition implements Testlet
{

  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
    test3(harness);
    test4(harness);
    test5(harness);
  }
  
  public void test1(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane();
    
    // Check that NPE is thrown when scrollpane does have a child.
    boolean fail = false;
    try
      {
        pane.setScrollPosition(0, 0);
      }
    catch (NullPointerException e)
      {
        fail = true;
      }
    harness.check(fail);
  }
  
  public void test2(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane();
    
    // Add a component (i.e. child) to the scrollpane.
    Button button = new Button();
    pane.add(button);
    harness.check(pane.getComponentCount(), 1);
    harness.check(pane.getComponent(0), button);
    
    // Check default scroll position.
    harness.check(pane.getScrollPosition(), new Point());
  }
  
  public void test3(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane();
    pane.add(new Button());
    
    // Check that setScrollPosition(int, int) and 
    // setScrollPosition(Point) return the same values.
    pane.setScrollPosition(1, 1);
    harness.check(pane.getScrollPosition(), new Point(0, 0));
    pane.setScrollPosition(new Point(1, 1));
    harness.check(pane.getScrollPosition(), new Point(0, 0));
  }
  
  public void test4(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane();
    pane.add(new Button());

    // Check that if x or y < 0, x and y are set to 0.
    pane.setScrollPosition(-1, -1);
    harness.check(pane.getScrollPosition(), new Point());
    pane.setScrollPosition(0, 0);
    harness.check(pane.getScrollPosition(), new Point());
  }
  
  public void test5(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane();
    Button button = new Button();
    button.setSize(100, 100);
    pane.add(button);
    harness.check(pane.getComponent(0).getWidth(), 100);
    harness.check(pane.getComponent(0).getHeight(), 100);
    harness.check(pane.getViewportSize().getWidth(), 100);
    harness.check(pane.getViewportSize().getHeight(), 100);
    
    // Check that if x > (child's width - viewport width), 
    // then x is set to (child's width - viewport width) and
    // that if y > (child's height - viewport height), 
    // then y is set to (child's height o veiwport height).
    int x = 100;
    int y = 100;
    int tempx = (int) (pane.getComponent(0).getWidth() - 
                          pane.getViewportSize().getWidth());
    int tempy = (int) (pane.getComponent(0).getHeight() -
                          pane.getViewportSize().getHeight());
    harness.check(tempx < x);
    harness.check(tempy < y);
    pane.setScrollPosition(x, y);
    harness.check(pane.getScrollPosition().getX(), tempx);
    harness.check(pane.getScrollPosition().getY(), tempy);
    
  }

}
