// Tags: JDK1.1
// Uses: rf_help rf2_help

// Copyright (C) 2002 Stephen Crawley <crawley@dstc.edu.au>

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

package gnu.testlet.java.lang.Class;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class reflect2 implements Testlet
{
  private TestHarness harness;
  private Class help;
  private Class help2;
  private Class help_inner;
  private Class help2_inner;
  private Class help2_inner_inner;

  public Class getClass(String name)
  {
    try {
      return Class.forName(name);
    }
    catch (Throwable ex) {
      ex.printStackTrace();
      return null;
    }
  }

  public void test_getClasses() 
  {
    harness.checkPoint("getClasses");

    Class[] inner = (new Object()).getClass().getClasses();
    harness.check(inner.length == 0);

    inner = help.getClasses();
    harness.check(inner.length == 1 && inner[0].equals(help_inner));

    inner = help2.getClasses();
    harness.check(inner.length == 3);

    inner = help_inner.getClasses();
    harness.check(inner.length == 0);

    inner = help2_inner.getClasses();
    harness.check(inner.length == 1 && inner[0].equals(help2_inner_inner));

    inner = help2_inner_inner.getClasses();
    harness.check(inner.length == 0);

  } 

  public void test_getDeclaringClass() 
  {
    harness.checkPoint("getDeclaringClass");

    Class outer = help.getDeclaringClass();
    harness.check(outer == null);

    outer = help2.getDeclaringClass();
    harness.check(outer == null);

    outer = help_inner.getDeclaringClass();
    harness.check(outer != null && outer.equals(help));

    outer = help2_inner.getDeclaringClass();
    harness.check(outer != null && outer.equals(help2));

    outer = help2_inner_inner.getDeclaringClass();
    harness.check(outer != null && outer.equals(help2_inner));
  }

  public void test_getDeclaredClasses() 
  {
    harness.checkPoint("getDeclaredClasses");

    Class[] inner = help.getDeclaredClasses();
    harness.check(inner.length == 1 && inner[0].equals(help_inner));    

    inner = help2.getDeclaredClasses();
    harness.check(inner.length == 8);
    
    inner = help2_inner.getDeclaredClasses();
    harness.check(inner.length == 1 && inner[0].equals(help2_inner_inner));

    inner = help2_inner_inner.getDeclaredClasses();
    harness.check(inner.length == 0);
  }

  public void test(TestHarness harness)
  {
    this.harness = harness;
    help = getClass("gnu.testlet.java.lang.Class.rf_help");
    help2 = getClass("gnu.testlet.java.lang.Class.rf2_help");

    help_inner = getClass("gnu.testlet.java.lang.Class.rf_help$inner");
    help2_inner = getClass("gnu.testlet.java.lang.Class.rf2_help$inner_class_1");
    help2_inner_inner = getClass("gnu.testlet.java.lang.Class."
		           + "rf2_help$inner_class_1$inner_inner_class_1");

    test_getClasses();
    test_getDeclaringClass();
    test_getDeclaredClasses();
  }
}
