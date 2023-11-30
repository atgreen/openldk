// Tags: JDK1.1
// Uses: ../sub/InvokeHelper iface

// Copyright (C) 1999, 2000, 2001, 2003, 2004 Cygnus Solutions

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
import gnu.testlet.java.lang.reflect.sub.InvokeHelper;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class invoke implements Testlet, iface
{
  int save = 0;

  // Don't use `.class' because gcj doesn't handle it yet.
  static Class tclass = null;

  static
  {
    try
      {
	tclass = Class.forName("java.lang.Throwable");
      }
    catch (Throwable _)
      {
	// Nothing.
      }
  }

  public String no_args ()
  {
    return "zardoz";
  }

  private String private_method ()
  {
    return "ok";
  }

  // Note that this is not overridden by InvokeHelper.p().
  String p ()
  {
    return "ppm";
  }

  public static int takes_int (int val)
  {
    if (val < 0)
      throw new IllegalArgumentException ();
    return val + 3;
  }

  public void returns_void (Integer val1, Integer val2)
  {
    save = val1.intValue() + val2.intValue();
  }

  public void try_invoke (TestHarness harness, Method method,
			  Object obj, Object[] args, Object expect)
  {
    Object result = null;
    try
      {
	result = method.invoke (obj, args);
      }
    catch (Throwable t)
      {
	result = t;
      }

    if (tclass.isInstance(expect))
      {
	// We're expecting an exception, so just make sure the types
	// match.
	harness.check(result.getClass(), expect.getClass());
      }
    else
      harness.check(result, expect);
  }

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

  public Method getDeclaredMethod (Class ic, String name, Class[] list)
  {
    Method m = null;
    try
      {
	m = ic.getDeclaredMethod(name, list);
      }
    catch (Throwable _)
      {
	// Nothing.
      }
    return m;
  }

  public void test (TestHarness harness)
  {
    // Don't use `.class' because gcj doesn't handle it yet.
    Class[] ic = new Class[2];
    try
      {
	ic[0] = Class.forName("gnu.testlet.java.lang.reflect.Method.invoke");
	ic[1] = Class.forName("gnu.testlet.java.lang.reflect.Method.iface");
      }
    catch (Throwable _)
      {
	// Just lose.
      }

    for (int i = 0; i < ic.length; ++i)
      {
	Class[] na_list = new Class[0];
	Method na_meth = getMethod (ic[i], "no_args", na_list);

	Class[] ti_list = new Class[1];
	ti_list[0] = Integer.TYPE;
	Method ti_meth = getMethod (ic[i], "takes_int", ti_list);

	Class[] rv_list = new Class[2];
	rv_list[0] = null;
	try
	  {
	    rv_list[0] = Class.forName("java.lang.Integer");
	  }
	catch (Throwable _)
	  {
	    // Just lose.
	  }
	rv_list[1] = rv_list[0];
	Method rv_meth = getMethod (ic[i], "returns_void", rv_list);

	harness.checkPoint ("no_args for " + ic[i]);
	Object[] args0 = new Object[0];
	// Successful invocation.
	try_invoke (harness, na_meth, this, args0, "zardoz");
	// Null `this' should fail.
	try_invoke (harness, na_meth, null, args0,
		    new NullPointerException ());
	if (! ic[i].isInterface())
	  {
	    // Too few arguments.
	    try_invoke (harness, ti_meth, this, args0,
			new IllegalArgumentException ());
	  }

	// Wrong class for `this'.
	try_invoke (harness, na_meth, new NullPointerException (),
		    args0, new IllegalArgumentException ());

	// null argument list is ok, at least according to JDK
	// implementation.
	try_invoke (harness, na_meth, this, null, "zardoz");

	if (! ic[i].isInterface())
	  {
	    harness.checkPoint ("takes_int for " + ic[i]);
	    Object[] args1 = new Object[1];
	    args1[0] = new Integer (5);
	    try_invoke (harness, na_meth, this, args1,
			new IllegalArgumentException ());
	    try_invoke (harness, ti_meth, this, args1, new Integer (8));
	    // null should work for object as this is a static method.
	    try_invoke (harness, ti_meth, null, args1, new Integer (8));
	    args1[0] = "joe louis";
	    try_invoke (harness, ti_meth, null, args1,
			new IllegalArgumentException ());
	    args1[0] = new Short ((short) 3);
	    try_invoke (harness, ti_meth, null, args1, new Integer (6));
	    args1[0] = new Long (72);
	    try_invoke (harness, ti_meth, null, args1,
			new IllegalArgumentException ());
	    args1[0] = null;
	    try_invoke (harness, ti_meth, null, args1,
			new IllegalArgumentException ());
	    args1[0] = new Integer (-5);
	    try_invoke (harness, ti_meth, null, args1,
			new InvocationTargetException (new IllegalArgumentException ()));
	  }

	harness.checkPoint ("returns_void for " + ic[i]);
	Object[] args2 = new Object[2];
	args2[0] = new Integer (7);
	args2[1] = new Integer (8);
	try_invoke (harness, rv_meth, this, args2, null);
	harness.check(save, 15);

	harness.checkPoint("invoke private method");
	Method pvm = getDeclaredMethod(ic[0], "private_method", null);
	try_invoke (harness, pvm, this, null, "ok");

	harness.checkPoint("invoke package-private method");
	Method ppvm = getDeclaredMethod(ic[0], "p", null);
	invoke sub = new InvokeHelper();
	try_invoke (harness, ppvm, sub, null, "ppm");
      }
  }
}
