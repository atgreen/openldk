/* TestPanelRepaint.java -- 
   Copyright (C) 2006 Red Hat
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


package gnu.testlet.java.awt.Panel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Frame;
import java.awt.Graphics;
import java.awt.List;
import java.awt.Panel;
import java.awt.Robot;
import java.awt.event.*;

public class TestPanelRepaint
    implements Testlet
{
  TestHarness harness;
  Robot r;
  boolean updateCalled;
  
  public void test(TestHarness harness)
  {
    this.harness = harness;
    r = harness.createRobot();
    myPanel p = new myPanel();
    p.add(new List(10));
    Frame f = new Frame();
    f.add(p);
    f.pack();
    f.show();
    
    // There is a delay to avoid any race conditions.    
    r.waitForIdle();
    r.delay(1000);
    
    f.move(100, 100);
    
    r.delay(3000);
    
    f.setSize(400, 400);
    
    // There is a delay so the tester can see the result.
    r.delay(3000);
    harness.check(updateCalled);

    // time to clean up
    f.dispose();
  }

  public class myPanel
      extends Panel implements ComponentListener
  {
    public myPanel()
    {
      super();
      addComponentListener(this);
    }

    public void update(Graphics g)
    {
      updateCalled = true;
      super.update(g);
    }

    public void componentResized(ComponentEvent e)
    {
      repaint();
    }

    public void componentMoved(ComponentEvent e)
    {
      repaint();
    }

    public void componentShown(ComponentEvent e)
    {
      repaint();
    }

    public void componentHidden(ComponentEvent e)
    {
      repaint();
    }
  }
}
