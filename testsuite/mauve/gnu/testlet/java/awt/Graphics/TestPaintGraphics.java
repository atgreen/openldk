// Tags: GUI JDK1.0

// Copyright (C) 2006 Red Hat

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

package gnu.testlet.java.awt.Graphics;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

/**
 * Tests that the graphics object passed to paint is different every
 * time.
 */
public class TestPaintGraphics implements Testlet
{
	boolean onePaintDone = false;
	int firstGraphicsHashCode = 0;
	int secondGraphicsHashCode = 0;
	
  public void test (TestHarness harness)
  {
    Frame f = new Frame ();
    Panel p = new Panel ()
    {
    	public void paint (Graphics g)
    	{
    		if (!onePaintDone)
    		{
    			firstGraphicsHashCode = System.identityHashCode(g);
    			onePaintDone = true;
    			repaint();
    		}
    		else
    		{
    			if (secondGraphicsHashCode == 0)
    			  secondGraphicsHashCode = System.identityHashCode(g);
    		}
    	}
    };
    
    f.add (p);
    f.setSize (200, 200);
    f.show ();
    Robot r = harness.createRobot();
    r.delay(2000);
    harness.check(firstGraphicsHashCode != 0 && secondGraphicsHashCode != 0);
    harness.check(firstGraphicsHashCode != secondGraphicsHashCode);  
  }
}
