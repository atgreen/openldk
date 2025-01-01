/* getLocationOnScreen.java -- Checks getLocationOnScreen()
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

// Tags: JDK1.0

package gnu.testlet.java.awt.Component;

import java.awt.Component;
import java.awt.Container;
import java.awt.Frame;
import java.awt.Point;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class getLocationOnScreen implements Testlet
{

  /**
   * Overrides Container and returns bogus values for getLocationOnScreen().
   */
  private class FakeContainer extends Container
  {

    public Point getLocationOnScreen()
    {
      return new Point(-1200, 12345);
    }
  }

  public void test(TestHarness harness)
  {
    testOverrideSafety(harness);

  }

  /**
   * Checks if that methods is safe from having components with overridden
   * getLocationOnScreen() methods in the tree.
   *
   * @param h the test harness
   */
  private void testOverrideSafety(TestHarness h)
  {
    // The heavyweight parent.
    Frame f = new Frame();

    // A lightweight container with a faked getLocationOnScreen() method.
    FakeContainer cont = new FakeContainer();

    // A lightweight component.
    Component comp = new Component(){};

    f.add(cont);
    cont.add(comp);

    f.setSize(100, 100);
    f.setVisible(true);

    cont.setBounds(10, 10, 80, 80);
    comp.setBounds(10, 10, 60, 60);


    Point frameLoc = f.getLocationOnScreen();
    // The component now should have
    // (frameLoc.x + 20 + i.left, frameLoc.y + 20 + i.top).
    Point compLoc = comp.getLocationOnScreen();
    h.check(compLoc.x, frameLoc.x + 20);
    h.check(compLoc.y, frameLoc.y + 20);

    f.dispose();
  }
}
