// Tags: JDK1.5
// Uses: Colour

// Copyright (C) 2007 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.java.util.EnumSet;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.util.EnumSet;

/**
 * Tests the {@link java.util.EnumSet#complementOf(java.util.EnumSet)}
 * method.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class ComplementOf
  implements Testlet
{

  public void test(TestHarness h)
  {
    /* Inverse of empty set should be full set */
    EnumSet<Colour> empty = EnumSet.noneOf(Colour.class);
    h.debug("Empty: " + empty);
    EnumSet<Colour> full = EnumSet.complementOf(empty);
    h.debug("Full: " + full);
    h.check(full.size() == Colour.class.getEnumConstants().length, "Inverse of empty is full");
    EnumSet<Colour> empty2 = EnumSet.complementOf(full);
    h.debug("Empty2: " + empty2);
    h.check(empty2.size() == 0, "Inverse of full is empty");
    h.check(empty.equals(empty2), "Inversing the inverse is same as original");
  }
}
