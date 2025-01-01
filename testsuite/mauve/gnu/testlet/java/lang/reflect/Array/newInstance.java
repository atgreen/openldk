// Tags: JDK1.1

// Copyright (C) 2000, 2001 Red Hat, Inc.

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

public class newInstance implements Testlet
{
  public void test(TestHarness harness)
  {

    harness.checkPoint("Void.TYPE");
    int val = 0;
    Object x = null;
    try
      {
	x = Array.newInstance(Void.TYPE, 10);
	val = 1;
      }
    catch (IllegalArgumentException iae)
      {
	val = 2;
      }
    catch (NullPointerException npe)
      {
	val = 4;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 2);
    try
      {
	harness.check(x, null); //Some Sun-based JDKs don't return null here
      }
    catch (InternalError ie)
      {
	harness.check(null, ie);
      }

    harness.checkPoint("Integer.TYPE");

    try
      {
	x = Array.newInstance(Integer.TYPE, 10);
	val = 1;
      }
    catch (IllegalArgumentException iae)
      {
	val = 2;
      }
    catch (NullPointerException npe)
      {
	val = 4;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 1);

    val = 999;

    try
      {
	val = ((int[]) x).length;
      }
    catch (ClassCastException cce)
      {
	val = 99;
      }
    harness.check(val, 10);

    val = 0;

    try
      {
	if (x.getClass().getComponentType() == Integer.TYPE)
	  val = 1;
      }
    catch (Throwable t)
      {
	val = 2;
      }
    harness.check(val, 1);

    harness.checkPoint("NegativeArraySize");

    try
      {
	x = Array.newInstance(String.class, -101);
	val = 1;
      }
    catch (NegativeArraySizeException nas)
      {
	val = 2;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 2);

    harness.checkPoint("multi-dimensional");
    val = 0;
    try
      {
	x = Array.newInstance(String.class, null);
	val = 1;
      }
    catch (NullPointerException e)
      {
	val = 2;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 3;
      }
    harness.check(val, 2);

    val = 0;
    try
      {
	x = Array.newInstance(String.class, new int[0]);
	val = 1;
      }
    catch (IllegalArgumentException e)
      {
	val = 2;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 3;
      }
    harness.check(val, 2);

    // This test is wrong: 255 is a potential limit, but not a
    // specified limit.
//      val = 0;
//      try
//        {
//  	x = Array.newInstance(String.class, new int[256]);
//  	val = 1;
//        }
//      catch (IllegalArgumentException e)
//        {
//  	val = 2;
//        }
//      catch (Throwable t)
//        {
//  	val = 3;
//        }
///    harness.check(val, 2);

    val = 0;
    try
      {
	// Some Sun JDKs croak, even though this is legal
	x = Array.newInstance(String.class, new int[255]);
	val = 1;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 2;
      }
    harness.check(val, 1);    

    val = 0;
    try
      {
	// Note that the non-reflective version, new String[0][-1], should
	// also fail, but does not on certain VMs; hence an additional test
	x = Array.newInstance(String.class, new int[] {0, -1});
	val = 1;
      }
    catch (NegativeArraySizeException e)
      {
	val = 2;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 3;
      }
    harness.check(val, 2);

    val = 0;
    try
      {
	x = new int[0][-1];
	val = 1;
      }
    catch (NegativeArraySizeException e)
      {
	val = 2;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 3;
      }
    harness.check(val, 2);

    val = 0;
    try
      {
	x = Array.newInstance(String.class, new int[]
	  {Integer.MAX_VALUE, Integer.MAX_VALUE});
	val = 1;
      }
    catch (OutOfMemoryError e)
      {
	val = 2;
      }
    catch (Throwable t)
      {
        harness.debug(t);
	val = 3;
      }
    harness.check(val, 2);

    harness.checkPoint("array");
    Class c = null;
    val = 0;
    try
      {
	// see above for why this is not 255
	x = Array.newInstance(String.class, new int[254]);
	c = x.getClass(); // faster than writing String[]...[].class
      }
    catch (Throwable t)
      {
	val = 1;
      }

    // This is invalid: it is ok to have an array with >255
    // dimensions.
//      try
//        {
//  	x = Array.newInstance(c, new int[] {1, 1});
//  	val = 2;
//        }
//      catch (IllegalArgumentException e)
//        {
//  	val = 3;
//        }
//      catch (Throwable t)
//        {
//  	val = 4;
//        }
//      harness.check(val, 3);

    try
      {
	x = Array.newInstance(c, 1);
	val = 5;
      }
    catch (Throwable t)
      {
	val = 6;
      }
    harness.check(val, 5);

    // Another invalid test.
//      try
//        {
//  	x = Array.newInstance(x.getClass(), 1);
//  	val = 7;
//        }
//      catch (IllegalArgumentException e)
//        {
//  	val = 8;
//        }
//      catch (Throwable t)
//        {
//  	val = 9;
//        }
//      harness.check(val, 8);

    // Also invalid.
//      try
//        {
//  	x = Array.newInstance(x.getClass(), new int[] {1, 1});
//  	val = 10;
//        }
//      catch (IllegalArgumentException e)
//        {
//  	val = 11;
//        }
//      catch (Throwable t)
//        {
//  	val = 12;
//        }
//      harness.check(val, 11);

    val = 0;
    try
      {
	x = Array.newInstance(int[].class, 1);
	val = 1;
	if (((int[][]) x).length == 1)
	  val = 2;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 2);

    harness.checkPoint("interface");
    val = 0;
    try
      {
	x = Array.newInstance(Runnable.class, 5);
	val = 1;
      }
    catch (Throwable t)
      {
	val = 2;
      }
    harness.check(val, 1);

    try
      {
	val = ((Runnable[]) x).length;
      }
    catch (ClassCastException cce)
      {
	val = 3;
      }
    harness.check(val, 5);

    val = 0;
    try
      {
	if (x.getClass().getComponentType() == Runnable.class)
	  val = 1;
	if (((Runnable[]) x)[0] == null)
	  val = 2;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 2);

    harness.checkPoint("String");
    x = "check";
    val = 0;
    try
      {
	x = Array.newInstance(String.class, 100);
	val = 1;
      }
    catch (IllegalArgumentException iae)
      {
	val = 2;
      }
    catch (NullPointerException npe)
      {
	val = 4;
      }
    catch (Throwable t)
      {
	val = 3;
      }
    harness.check(val, 1);

    try
      {
	val = ((String[]) x).length;
      }
    catch (ClassCastException cce)
      {
	val = 99;
      }
    harness.check(val, 100);
    harness.debug(x.getClass().toString());

    val = 0;
    try
      {
	if (x.getClass().getComponentType() == String.class
	    && ((String[]) x)[0] == null)
	  val = 1;
      }
    catch (Throwable t)
      {
	val = 2;
      }
    harness.check(val, 1);
    
  }
}
