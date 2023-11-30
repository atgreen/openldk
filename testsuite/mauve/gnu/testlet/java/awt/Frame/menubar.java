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


package gnu.testlet.java.awt.Frame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;

public class menubar implements Testlet
{
  public void test (TestHarness harness)
  {
    Frame f = new Frame();
    harness.check(f.getMenuBar(), null);

    MenuBar mb = new MenuBar();
    f.setMenuBar(mb);
    harness.check(f.getMenuBar(), mb);
    harness.check(mb.getParent(), f);

    f.remove(mb);
    harness.check(f.getMenuBar(), null);
    harness.check(mb.getParent(), null);
    
    f.setMenuBar(mb);
    harness.check(f.getMenuBar(), mb);
    harness.check(mb.getParent(), f);

    f.setMenuBar(null);
    harness.check(f.getMenuBar(), null);
    harness.check(mb.getParent(), null);
  }
}
