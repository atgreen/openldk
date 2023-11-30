// Tags: GUI JDK1.2

// Copyright (C) 2005 Roman Kennke

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

package gnu.testlet.java.awt.Component;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import javax.swing.*;

/**
 * Checks if the update method in Component is called correctly. The
 * update method is only called on lightweight and top-level components.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class update implements Testlet
{
  /**
   * The testclasses defined in this test append a unique character to
   * this teststring. This way we can check which component's update and paint
   * method gets called and in which order.
   */
  StringBuffer test = new StringBuffer();

  // We define some classes here, they all override update() to append
  // a unique character to the teststring. At the end we test if the
  // teststring is correct.
  class TopLevel extends Frame
  {
    public void update(Graphics g)
    {
      test.append('1');
      super.update(g);
    }
    public void paint(Graphics g)
    {
      test.append('2');
      super.paint(g);
    }
  }

  class LightWeight extends Component
  {
    public void update(Graphics g)
    {
      test.append('3');
      super.update(g);
    }
    public void paint(Graphics g)
    {
      test.append('4');
      super.paint(g);
    }
    public boolean isLightweight()
    {
      return true;
    }
  }
  
  class HeavyWeight extends Label
  {
    public void update(Graphics g)
    {
      test.append('5');
      super.update(g);
    }
    public void paint(Graphics g)
    {
      test.append('6');
      super.paint(g);
    }
    public boolean isLightweight()
    {
      return false;
    }
  }

  class LightContainer extends Container
  {
    public void update(Graphics g)
    {
      test.append('7');
      super.update(g);
    }
    public void paint(Graphics g)
    {
      test.append('8');
      super.paint(g);
    }
  }

  class HeavyContainer extends Label
  {
    public void update(Graphics g)
    {
      test.append('a');
      super.update(g);
    }
    public void paint(Graphics g)
    {
      test.append('b');
      super.paint(g);
    }
  }

  /**
   * This Graphics subclass is used to check if the background is cleared
   * in the update method.
   */
  class TestGraphics extends DebugGraphics
  {
    TestGraphics(Graphics g)
    {
      super(g);
    }
    public void clearRect(int x, int y, int w, int h)
    {
      test.append('9');
    }
  }

  public void test (TestHarness harness)
  {
    TopLevel t = new TopLevel();
    t.setLayout(new GridLayout());
    
    HeavyWeight h = new HeavyWeight();
    t.add(h);
    LightWeight l = new LightWeight();
    t.add(l);
    LightContainer c = new LightContainer();
    t.add(c);
    HeavyContainer c2 = new HeavyContainer();
    t.add(c2);

    t.setSize(200, 200);
    t.setVisible(true);
    Graphics g = new TestGraphics(t.getGraphics());

    // Wait until frame has become visible.
    try
      {
        Thread.sleep(3000);
      }
    catch (Exception ex)
      {
      }

    test = new StringBuffer();
    t.update(g);
    harness.check(test.toString(), "19284", test.toString());
    t.setVisible(false);
    t.dispose();
    
  }
}
