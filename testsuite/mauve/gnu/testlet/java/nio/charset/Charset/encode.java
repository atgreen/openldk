// Tags: JDK1.4

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

package gnu.testlet.java.nio.charset.Charset;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.*;
import java.nio.charset.*;

public class encode implements Testlet
{
  private void checkByteBuffer(TestHarness h, ByteBuffer bb, int capacity, int position, int limit)
  {
    h.check(bb != null, "Byte buffer is null");
    h.check(bb.capacity(), capacity, "Wrong capacity in byte buffer");
    h.check(bb.limit(), limit, "Wrong limit in byte buffer");
  }
  
  private void checkCharBuffer(TestHarness h, CharBuffer cb, int capacity, int position, int limit)
  {
    h.check(cb != null, "Char buffer is null");
    h.check(cb.capacity(), capacity, "Wrong capacity in char buffer");
    h.check(cb.position(), position, "Wrong position in char buffer");
    h.check(cb.limit(), limit, "Wrong limit in char buffer");
  }
  
  public void test(TestHarness h)
  {
    Charset cs1 = Charset.forName("UTF-16");
    Charset cs2 = Charset.forName("US-ASCII");

    ByteBuffer bb;
    CharBuffer cb = CharBuffer.wrap("Hello World! Hello World! Hello World!");
    
    bb = cs1.encode(cb);
    checkByteBuffer(h, bb, 152, 0, 78);
    checkCharBuffer(h, cb, 38, 38, 38);

    bb = cs2.encode(cb);
    checkByteBuffer(h, bb, 0, 0, 0);

  }
}
