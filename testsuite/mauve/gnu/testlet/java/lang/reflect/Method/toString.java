// Tags: JDK1.1

// Copyright (C) 1999, 2000, 2001, 2004 Red Hat, Inc.

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

package gnu.testlet.java.lang.reflect.Method;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.Method;

public class toString implements Testlet
{
  public Method getMethod (Class ic, String name, Class[] list)
  {
    Method m = null;
    try
      {
	m = ic.getMethod(name, list);
      }
    catch (Throwable _)
      {
	// Nothing.
      }
    return m;
  }

  public String no_args ()
  {
    return "zardoz";
  }

  public void simpleargs (int i, byte b)
  {
  }
  
  public int arrayargs (int[] z)
  {
    return z.length;
  }

  public int multidim_arrayargs (int[][][] z)
  {
    return z.length;
  }

  public String classargs (String[][] s)
  {
    return "test";
  }

  public String[] arrayreturn()
  {
    return null;
  }

  public void test (TestHarness harness)
  {
    Class ic = null;
    try
      {
	ic = Class.forName ("gnu.testlet.java.lang.reflect.Method.toString");
      }
    catch (Throwable _)
      {
	// Lose.
      }

    Class[] na_list = new Class[0];
    Method na_meth = getMethod (ic, "no_args", na_list);
    harness.checkPoint("method with no arguments");
    harness.check (na_meth.toString (), "public java.lang.String gnu.testlet.java.lang.reflect.Method.toString.no_args()");

    Class[] simple_list = new Class[2];
    simple_list[0] = int.class;
    simple_list[1] = byte.class;
    Method simple_meth = getMethod (ic, "simpleargs", simple_list);
    harness.checkPoint("method with primitive argument types");
    harness.check (simple_meth.toString (),
		    "public void gnu.testlet.java.lang.reflect.Method.toString.simpleargs(int,byte)");
    
    Class[] aa_list = new Class[1];
    aa_list[0] = int[].class;
    Method aa_meth = getMethod (ic, "arrayargs", aa_list);
    harness.checkPoint("method with a simple array type as argument");
    harness.check (aa_meth.toString (),
		   "public int gnu.testlet.java.lang.reflect.Method.toString.arrayargs(int[])");

    Method mdaa_meth = getMethod (ic, "multidim_arrayargs", aa_list);
    harness.check(mdaa_meth == null, "invalid argument list for this method, getMethod should return null");

    aa_list = new Class[1];
    aa_list[0] = int[][][].class;
    mdaa_meth = getMethod (ic, "multidim_arrayargs", aa_list);

    harness.checkPoint("method with multiple array dims in argument");
    harness.check (mdaa_meth.toString (),
		   "public int gnu.testlet.java.lang.reflect.Method.toString.multidim_arrayargs(int[][][])");

    aa_list = new Class[1];
    aa_list[0] = String[][].class;
    Method string_meth = getMethod (ic, "classargs", aa_list);
    harness.checkPoint("method with class type array");
    harness.check (string_meth.toString (),
		    "public java.lang.String gnu.testlet.java.lang.reflect.Method.toString.classargs(java.lang.String[][])");

    na_list = new Class[0];
    Method ra_meth = getMethod (ic, "arrayreturn", na_list);
    harness.checkPoint("method with array as return type");
    harness.check (ra_meth.toString (),
		    "public java.lang.String[] gnu.testlet.java.lang.reflect.Method.toString.arrayreturn()");
  }
}
