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


package gnu.testlet.java.awt.MenuBar;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

public class addMenu implements Testlet
{
  public void test (TestHarness harness)
  {
    MenuBar mb = new MenuBar();
    harness.check(mb.countMenus(), 0);

    Menu m = new Menu("Testing");
    mb.add(m);
    harness.check(mb.countMenus(), 1);
    harness.check(mb.getMenu(0), m);
    harness.check(m.getParent(), mb);

    mb.remove(m);
    harness.check(mb.countMenus(), 0);
    harness.check(m.getParent(), null);

    mb.add(m);
    harness.check(mb.countMenus(), 1);
    harness.check(mb.getMenu(0), m);
    harness.check(m.getParent(), mb);
    
    Menu m2 = new Menu("SecondMenu");
    mb.add(m2);
    harness.check(mb.countMenus(), 2);
    harness.check(mb.getMenu(0), m);
    harness.check(mb.getMenu(1), m2);
    harness.check(m2.getParent(), mb);

    mb.remove(0);
    harness.check(mb.countMenus(), 1);
    harness.check(mb.getMenu(0), m2);
    harness.check(m.getParent(), null);
    harness.check(m2.getParent(), mb);

    mb.remove(m2);
    harness.check(mb.countMenus(), 0);
    harness.check(m2.getParent(), null);
  }
}
