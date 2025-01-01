// Tags: GUI JDK1.0

// Copyright (C) 2004 Red Hat

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

package gnu.testlet.java.awt.MenuItem;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.*;
import java.awt.event.*;

/**
 * Check initial value of a MenuItem label.
 */
public class label1 implements Testlet
{
  public void test (TestHarness harness)
  {
    MenuItem m1 = new MenuItem ();

    harness.check (m1.getLabel() == "");

    MenuItem m2 = new MenuItem ("menu item 2");

    harness.check (m2.getLabel() == "menu item 2");

    MenuItem m3 = new MenuItem (null);

    harness.check (m3.getLabel() == null);
  }
}
