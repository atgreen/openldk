// Tags: 1.4

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
// along with Mauve; see the file COPYING.  If not, write to the Free
// Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA 02110-1301 USA.

package gnu.testlet.java.awt.ScrollPane;

import java.awt.*;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Test ScrollPane's add method.
 */
public class add implements Testlet
{
  private static boolean layoutCalled;

  public void test(TestHarness harness)
  {
    ScrollPane pane = new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS)
      {
        public void layout()
        {
          layoutCalled = true;
        };
      };

    Button b = new Button ("Hello");
    b.setSize(400, 400);
    pane.add(b);

    harness.check(!layoutCalled);

    Frame f = new Frame("add");

    f.setSize(300, 300);
    f.add(pane);

    harness.check(!layoutCalled);

    f.show();

    harness.check(layoutCalled);

    harness.check(!(b.getParent() instanceof Panel));

    // Check parent when adding a Panel.
    ScrollPane pane2 = new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS);

    Frame f2 = new Frame("add2");

    Panel p = new Panel();

    p.setSize(400, 400);
    pane2.add(p);
    f2.setSize(300, 300);
    f2.add(pane2);

    f2.show();
    harness.check(!(p.getParent() instanceof Panel));

    // Check parent when adding a lightweight component.
    ScrollPane pane3 = new ScrollPane(ScrollPane.SCROLLBARS_ALWAYS);

    Frame f3 = new Frame("add2");

    Component c = new Component() {};

    c.setSize(400, 400);
    pane3.add(c);
    f3.setSize(300, 300);
    f3.add(pane3);

    f3.show();
    harness.check(c.getParent() instanceof Panel);

    // it's necesarry to clean up
    f.dispose();
    f2.dispose();
    f3.dispose();
  }
}
