// Tags: JDK1.1

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Mark Wielaard (mark@klomp.org)

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

package gnu.testlet.java.lang.reflect.AccessibleObject;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.lang.reflect.*;
import java.io.*;

public class accessible implements Testlet
{
  public void test (TestHarness harness)
  {
    try
      {
	Class cl_class = Class.forName("java.lang.ClassLoader");
	
	// There is a protected no-argument ClassLoader() constructor.
	Class[] params = new Class[0];
	Constructor cl_cons = cl_class.getDeclaredConstructor(params);
	harness.check(!cl_cons.isAccessible());
	
	cl_cons.setAccessible(true);
	harness.check(cl_cons.isAccessible());
	
	// Get a new Constructor to check that it isn't accessible.
	cl_cons = cl_class.getDeclaredConstructor(params);
	harness.check(!cl_cons.isAccessible());

	// ClassLoader.findLoadedClass(String) is a protected method.
	params = new Class[1];
	params[0] = Class.forName("java.lang.String");
	Method m = cl_class.getDeclaredMethod("findLoadedClass", params);
	harness.check(!m.isAccessible());

	m.setAccessible(true);
	harness.check(m.isAccessible());

	// Get a new Member to check that is isn't accessible.
	m = cl_class.getDeclaredMethod("findLoadedClass", params);
        harness.check(!m.isAccessible());
       
	// Take some random protected field from DataInputStream.
	DataInputStream dis
	  = new DataInputStream(new ByteArrayInputStream(new byte[0]));	
	Class dis_cl = dis.getClass();
	Class fis_cl = dis_cl.getSuperclass();
	Field dis_f = fis_cl.getDeclaredField("in");
	harness.check(!dis_f.isAccessible());

	dis_f.setAccessible(true);
	harness.check(dis_f.isAccessible());

	// Get a new Field to check that is isn't accessible.
	dis_f = fis_cl.getDeclaredField("in");
	harness.check(!dis_f.isAccessible());
	
      }
    catch (Throwable t)
      {
	harness.debug(t);
	harness.fail(t.toString());
      }
  }
}
