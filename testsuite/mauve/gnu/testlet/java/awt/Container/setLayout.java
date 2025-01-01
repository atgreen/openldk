/* setLayout.java -- Checks how setLayout is supposed to work
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

// Tags: JDK1.1

package gnu.testlet.java.awt.Container;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setLayout implements Testlet
{

  private boolean invalidated;

  private class TestContainer extends Container
  {
    public void invalidate()
    {
      super.invalidate();
      invalidated = true;
    }
  }

  public void test(TestHarness harness)
  {
    testInvalidate(harness);
  }

  private void testInvalidate(TestHarness h)
  {
    h.checkPoint("invalidate");
    TestContainer c = new TestContainer();

    // Test not showing.
    // Valid components get invalidated.
    c.validate();
    h.check(! c.isValid());
    invalidated = false;
    c.setLayout(new FlowLayout());
    h.check(! invalidated);

    // Invalid components don't get invalidated.
    h.check(!c.isValid());
    invalidated = false;
    c.setLayout(new FlowLayout());
    h.check(! invalidated);

    // Test showing.
    Frame f = new Frame();
    f.add(c);
    f.setSize(100, 100);
    f.setVisible(true);

    h.check(c.isShowing());
    // Valid components get invalidated.
    c.validate();
    h.check(c.isValid());
    invalidated = false;
    c.setLayout(new FlowLayout());
    h.check(invalidated);

    // Invalid components don't get invalidated.
    h.check(!c.isValid());
    invalidated = false;
    c.setLayout(new FlowLayout());
    h.check(! invalidated);

    // time to clean up
    f.dispose();
  }
}
