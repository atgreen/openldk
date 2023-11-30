// Tags: JDK1.2

// Copyright (C) 2004 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.plaf.metal.DefaultMetalTheme;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import javax.swing.plaf.*;
import javax.swing.plaf.metal.*;

public class DefaultMetalThemeTest extends DefaultMetalTheme implements Testlet
{
  private void check(TestHarness h, ColorUIResource color, int r, int g, int b)
  {
    h.check(color.getRed(), r, "red color component");
    h.check(color.getGreen(), g, "green color component");
    h.check(color.getBlue(), b, "blue color component");
  }
  
  public void test(TestHarness h)
  {
    h.check(getName(), "Steel", "name of theme");

    check(h, getPrimary1(), 102, 102, 153);
    check(h, getPrimary2(), 153, 153, 204);
    check(h, getPrimary3(), 204, 204, 255);
    check(h, getSecondary1(), 102, 102, 102);
    check(h, getSecondary2(), 153, 153, 153);
    check(h, getSecondary3(), 204, 204, 204);
  }
}
