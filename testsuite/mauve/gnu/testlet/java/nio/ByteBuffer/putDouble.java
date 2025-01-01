// Tags: JDK1.4

// Copyright (C) 2004 Sven de Marothy

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

package gnu.testlet.java.nio.ByteBuffer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.ByteBuffer;

public class putDouble
  implements Testlet
{
  private TestHarness h;

  public void test(TestHarness h)
  {
      ByteBuffer b = ByteBuffer.allocate(8);
      long doubleBits = 0x7ff8000000000007L;
      double d = Double.longBitsToDouble(doubleBits);
      System.out.println(Double.isNaN(d));
      b.putDouble(d);

      // Test 1: Check the bit pattern, should match RawLongBits exactly
      h.check(doubleBits == Double.doubleToRawLongBits(b.getDouble(0)));
      
      // Test 2: Try an ordinary number (exactly representable in binary)
      b.putDouble(0, 1.5);
      h.check(1.5 == b.getDouble(0));
      
  }

}

