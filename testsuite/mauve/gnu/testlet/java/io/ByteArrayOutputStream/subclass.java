// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation, Inc.

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

package gnu.testlet.java.io.ByteArrayOutputStream;

import java.io.ByteArrayOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class subclass extends ByteArrayOutputStream implements Testlet
{
  public subclass ()
  {
    super (10);
  }

  public void test (TestHarness harness)
  {
    for (int n = 0; n < 10; n++)
      write (n);

    // Ensure that writing 10 bytes to a stream with capacity 10
    // does not cause it to grow.
    harness.check(count, 10, "count");
    harness.check(buf.length, 10, "buf.length");
    for (int n = 0; n < 10; n++)
      harness.check (buf[n], n, "buf[" + n + "]");
  }
}
