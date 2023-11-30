// Tags: JDK1.2

// Copyright (C) 2003 Daniel Bonniot <bonniot@users.sf.net>

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

package gnu.testlet.java.awt.color.ColorSpace;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.awt.color.ColorSpace;

/**
 * Checks that isCS_sRGB returns true for ColorSpace.CS_sRGB.
 */
public class isCS_sRGB implements Testlet
{
  public void test (TestHarness harness)
  {
    harness.check(ColorSpace.getInstance(ColorSpace.CS_sRGB).isCS_sRGB());
  }
}
