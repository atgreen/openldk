/* Copyright (C) 2001 Eric Blake

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

// Tags: JDK1.0

package gnu.testlet.java.lang.Object;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * This class tests OutOfMemoryError condition.
 */
public final class oom implements Testlet
{
  public void test (TestHarness harness)
  {
    // This test sees if clone can cause an out of memory error.
    // The testlet should be able to recover, though!
    Object[] oarray = new Object[5];
    int[] iarray = null;
    try
      {
	int size = 1024;
	while (true)
	  {
	    size <<= 1;
	    iarray = new int[size];
	  }
      }
    catch (OutOfMemoryError oome)
      {
	// we should still have memory left, since the last allocation
	// attempt failed; but multiple clones should push us over
	try
	  {
	    oarray[0] = iarray.clone();
	    oarray[1] = iarray.clone();
	    oarray[2] = iarray.clone();
	    oarray[3] = iarray.clone();
	    oarray[4] = iarray.clone();
	    harness.fail("clone didn't cause expected OutOfMemoryError");
	  }
	catch (OutOfMemoryError e)
	  {
	    harness.check(true, "clone can exceed memory");
	  }
	oarray = null; // help free memory
      }
  }
}
