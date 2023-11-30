// Tags: JDK1.5

// Copyright (C) 1998 Cygnus Solutions

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

package gnu.testlet.java.lang.Byte;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class new_Byte implements Testlet
{
  public void test (TestHarness harness)
    {
      Byte a = new Byte((byte) 0);
      Byte b = new Byte((byte) 1);
      Byte c = new Byte(Byte.MAX_VALUE);
      Byte d = new Byte((byte) -1);
      Byte e = new Byte(Byte.MIN_VALUE);

      harness.check (a.hashCode(), 0);
      harness.check (b.hashCode(), 1);
      harness.check (c.hashCode(), 127);
      harness.check (d.hashCode(), -1);
      harness.check (e.hashCode(), -128);
    }
}
