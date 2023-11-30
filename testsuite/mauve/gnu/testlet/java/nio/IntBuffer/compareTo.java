// Tags: JDK1.4

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>

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

package gnu.testlet.java.nio.IntBuffer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.IntBuffer;

public class compareTo
  implements Testlet
{
  private TestHarness h;

  public void test(TestHarness h)
  {
    this.h = h;

    int[] a = { 1, 2, 3, 4, 5, };
    int[] b = { 1, 2, 3, 4, 5, 6, 7, 8, 9, };
    int[] c = { 1, 2, 3, 5, 7, 9, };
    
    // =
    checkCompareTo(a, a, 0);
    checkCompareTo(b, b, 0);
    checkCompareTo(c, c, 0);
    
    // <
    checkCompareTo(a, b, -1);
    checkCompareTo(a, c, -1);
    checkCompareTo(b, c, -1);

    // >
    checkCompareTo(b, a, 1);
    checkCompareTo(c, a, 1);
    checkCompareTo(c, b, 1);
  }

  private void checkCompareTo(int[] buf1, int[] buf2, int expected)
  {
    IntBuffer b1 = IntBuffer.wrap(buf1);
    IntBuffer b2 = IntBuffer.wrap(buf2);

    int got = b1.compareTo(b2);
    int real_got = got;

    if (got > 1) got = 1;
    if (got < -1) got = -1;
    
    h.check(got == expected, "expected: " + expected + ", got: " + real_got);
  }
}

