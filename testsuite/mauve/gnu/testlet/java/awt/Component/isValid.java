/* isValid.java -- Checks how isValid() is supposed to work
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

// Tags:JDK1.1

package gnu.testlet.java.awt.Component;

import java.awt.Component;
import java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class isValid implements Testlet
{

  public void test(TestHarness harness)
  {
    testShowing(harness);
    testNotShowing(harness);
  }

  private void testShowing(TestHarness h)
  {
    h.checkPoint("showing");
    Frame f = new Frame();
    Component c = new Component(){};
    f.add(c);
    f.setSize(100, 100);
    f.setVisible(true);

    c.invalidate();
    h.check(!c.isValid());
    c.validate();
    h.check(c.isValid());
  }

  private void testNotShowing(TestHarness h)
  {
    h.checkPoint("notShowing");

    Component c = new Component(){};

    c.invalidate();
    h.check(!c.isValid());
    c.validate();
    h.check(!c.isValid());
  }
}
