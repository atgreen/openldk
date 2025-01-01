// Tags: JDK1.1

// Copyright (C) 2003 Red Hat, Inc.

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

package gnu.testlet.java.lang.reflect.Array;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Array;

public class set implements Testlet
{
  public void test(TestHarness harness)
  {
    Throwable[] x = new Throwable[5];
    Array.set (x, 0, null);
    harness.check (x[0], null);

    Throwable t = new Throwable();
    Array.set (x, 1, t);
    harness.check(x[1], t);

    Exception e = new Exception();
    Array.set (x, 2, e);
    harness.check(x[2], e);

    Object o = new Object();
    boolean exception_thrown = false;
    try
      {
	Array.set (x, 3, o);
      }
    catch (IllegalArgumentException iae)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);
    harness.check(x[3], null);

    String s = "string";
    exception_thrown = false;
    try
      {
	Array.set (x, 4, s);
      }
    catch (IllegalArgumentException iae)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);
    harness.check(x[4], null);

    exception_thrown = false;
    try
      {
	Array.set (x, 5, t);
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
	exception_thrown = true;
      }
    harness.check(exception_thrown);
  }
}
