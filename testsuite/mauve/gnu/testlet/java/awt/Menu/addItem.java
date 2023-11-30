// Tags: GUI JDK1.0

// Copyright (C) 2006 Free Software Foundation, Inc.

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
// Software Foundation,Inc., 51 Franklin Street, Fifth Floor, Boston,
// MA 02110-1301 USA.


package gnu.testlet.java.awt.Menu;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

public class addItem implements Testlet
{
  public void test (TestHarness harness)
  {
    Menu m = new Menu("Test");
    harness.check(m.countItems(), 0);

    MenuItem i = new MenuItem("Testing");
    m.add(i);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);

    m.remove(i);
    harness.check(m.countItems(), 0);
    harness.check(i.getParent(), null);

    m.add(i);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);
    
    MenuItem i2 = new MenuItem("Second");
    m.insert(i2, 0);
    harness.check(m.countItems(), 2);
    harness.check(m.getItem(0), i2);
    harness.check(m.getItem(1), i);
    harness.check(i.getParent(), m);
    harness.check(i2.getParent(), m);

    m.remove(0);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);
    harness.check(i2.getParent(), null);

    m.remove(i);
    harness.check(m.countItems(), 0);
    harness.check(i.getParent(), null);

    // A Menu can be an MenuItem
    
    i = new Menu("TestingMenu");
    m.add(i);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);

    m.remove(i);
    harness.check(m.countItems(), 0);
    harness.check(i.getParent(), null);

    m.add(i);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);
    
    i2 = new Menu("SecondMenu");
    m.insert(i2, 0);
    harness.check(m.countItems(), 2);
    harness.check(m.getItem(0), i2);
    harness.check(m.getItem(1), i);
    harness.check(i.getParent(), m);
    harness.check(i2.getParent(), m);

    m.remove(0);
    harness.check(m.countItems(), 1);
    harness.check(m.getItem(0), i);
    harness.check(i.getParent(), m);
    harness.check(i2.getParent(), null);

    m.remove(i);
    harness.check(m.countItems(), 0);
    harness.check(i.getParent(), null);
  }
}
