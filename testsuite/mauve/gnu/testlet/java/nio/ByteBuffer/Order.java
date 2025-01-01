// Tags: JDK1.4

// Copyright (C) 2004 Max Gilead <gilead@yellowhedgehog.com>

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

package gnu.testlet.java.nio.ByteBuffer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class Order implements Testlet
{
  public void test(TestHarness h)
  {
    ByteBuffer buf = ByteBuffer.allocate(4);
    h.check(buf.order(ByteOrder.BIG_ENDIAN), buf, "buf.order(ByteOrder.BIG_ENDIAN)");
    h.check(buf.order(), ByteOrder.BIG_ENDIAN, "order() == ByteOrder.BIG_ENDIAN");
    buf.putInt(0x11223344);
    h.check(buf.order(ByteOrder.LITTLE_ENDIAN), buf, "buf.order(ByteOrder.LITTLE_ENDIAN)");
    h.check(buf.order(), ByteOrder.LITTLE_ENDIAN, "order() == ByteOrder.LITTLE_ENDIAN");
    buf.rewind();
    h.check(buf.getInt(), 0x44332211, "get()");
  }
}
