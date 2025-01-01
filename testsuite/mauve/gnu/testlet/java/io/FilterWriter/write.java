// Tags: JDK1.1
// Uses: MyFilterWriter

// Copyright (C) 2002 Free Software Foundation, Inc.
// Written by Daryl Lee (dolee@sources.redhat.com)

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

package gnu.testlet.java.io.FilterWriter;

import java.io.CharArrayWriter;
import java.io.FilterWriter;
import java.io.IOException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class write implements Testlet
{
    
  public void test (TestHarness harness)
  {
    CharArrayWriter caw = new CharArrayWriter();
    FilterWriter tfw = new MyFilterWriter(caw);
    try {
      tfw.write('A');						// A
      harness.check(true, "write(int)");
      char[] ba = {'A', 'B', 'C', 'D'};
      tfw.write(ba, 1, 2);				// ABC
      harness.check(true, "write(buf,off,len)");
      tfw.write("CDEF", 1, 3);			// ABCDEF
      harness.check(caw.toString(), "ABCDEF", "wrote all characters okay");
      tfw.flush();
      harness.check(true, "flush()");
      tfw.close();
      harness.check(true, "close()");
    }
    catch (IOException e) {
      harness.debug(e);
      harness.fail("IOException unexpected");
    }

    try {
      // The documented JDK 1.4 behaviour is to throw NullPointerException
      // if the constructor arg is null.
      new MyFilterWriter(null);
      harness.check(false, "new MyFilterWriter(null) -> no exception");
    }
    catch (NullPointerException ex) {
      harness.check(true, "new MyFilterWriter(null) -> NullPointerException");
    }
  }
}
