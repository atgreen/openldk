// Tags: JDK1.5
// Uses: DisabledEventQueue

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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

package gnu.testlet.java.awt.Container;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Label;
import java.awt.LayoutManager;
import java.awt.Panel;
import java.awt.Toolkit;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;

/**
 * This checks tests if the addImpl method notfies container listeners when
 * a container is not showing and if the notification is sent over the event
 * queue or delivered directly. The test shows that the event is also sent when
 * the container is not showing and that the event is delivered directly.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class addImpl implements Testlet
{

  /**
   * A container listener for test.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  class TestContainerListener implements ContainerListener
  {

    /**
     * Receives notification when a component was added to the container.
     *
     * @param event the container event
     */
    public void componentAdded(ContainerEvent event)
    {
      componentAddedCalled = true;
    }

    /**
     * Receives notification when a component was removed from the container.
     *
     * @param event the container event
     */
    public void componentRemoved(ContainerEvent event)
    {
      // TODO Auto-generated method stub
      
    }
    
  }

  /**
   * True when the componentAdded method in the container listener was called.
   */
  boolean componentAddedCalled;

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    test1(harness);
    test2(harness);
  }
  
  public void test1(TestHarness harness)
  {
    // We disable the event queue so we can check if this event is delivered
    // via the event queue or not.
    Toolkit.getDefaultToolkit().getSystemEventQueue().push(new DisabledEventQueue());

    final TestHarness transfer = harness;
    Container c = new Container()
    {
      TestHarness harness = transfer;
            
      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    c.addContainerListener(new TestContainerListener());
    // Pre-condition check.
    harness.check(c.isShowing(), false);
    componentAddedCalled = false;
    
    Component a = new Component()
    {
      TestHarness harness = transfer;

      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    Component b = new Component()
    {
      TestHarness harness = transfer;

      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    c.add(a);
    harness.check(componentAddedCalled, true);
    
    // check that LayoutManager.addLayoutComponent() is called
    // with non-null name.
    Container two = new Container()
    {
      TestHarness harness = transfer;
            
      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    two.setLayout(new LayoutManager()
    {
      TestHarness harness = transfer;

      Dimension size = new Dimension();

      public void removeLayoutComponent(Component comp)
      {
      }

      public java.awt.Dimension preferredLayoutSize(Container cont)
      {
        return size;
      }

      public Dimension minimumLayoutSize(Container cont)
      {
        return size;
      }

      public void layoutContainer(Container container)
      {
      }

      public void addLayoutComponent(String name, Component comp)
      {
        boolean pass = (name != null);
        harness.check(pass, true);
      }
    });
    harness.check(two.isShowing(), false);
    componentAddedCalled = false;
    two.addContainerListener(new TestContainerListener());
    two.add(b);
    harness.check(componentAddedCalled, true);
  }
  
  public void test2(TestHarness harness)
  {
    final TestHarness transfer = harness;
    Frame f = new Frame()
    {
      TestHarness harness = transfer;

      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    Panel a = new Panel()
    {
      TestHarness harness = transfer;
            
      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    Label l = new Label("!!!!!")
    {
      TestHarness harness = transfer;

      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    Container c = new Container()
    {
      TestHarness harness = transfer;

      public void repaint(long tm, int x, int y, int w, int h)
      {
        harness.fail("repaint has been called.");
      }
    };
    
    a.add(c);
    a.add(l);
    c.setSize(100,100);
    f.add(a);
    f.pack();
    f.show();

    harness.check(a.isShowing(), true);
    harness.check(c.isShowing(), true);
    harness.check(l.isShowing(), true);
    harness.check(f.isShowing(), true);
    harness.check(c.isLightweight(), true);
    harness.check(a.isLightweight(), false);
    harness.check(l.isLightweight(), false);

    // clean up the frame from desktop
    f.dispose();
  }
}
