// Tags: JDK1.1

// Uses: pkg/test1 pkg/test2 pkg/test3 pkg/test4

// Copyright (C) 2005 Jeroen Frijters
// Copyright (C) 2006 Mark J. Wielaard

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

import java.io.IOException;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class newInstance implements Testlet
{
  static class test1
  {
    private static class inner 
    {
      public inner()
      {
      }
    }

    test1()
    {
    }

    static void check(TestHarness harness)
    {
      try
        {
          harness.check(test1.class.isInstance(test1.class.newInstance()));
        }
        catch (Throwable t)
        {
          harness.debug(t);
          harness.check(false);
        }
    }
  }

  public static class test2
  {
    public test2()
    {
    }

    static void check(TestHarness harness)
    {
      try
        {
          harness.check(test2.class.isInstance(test2.class.newInstance()));
        }
        catch (Throwable t)
        {
          harness.debug(t);
          harness.check(false);
        }
    }
  }

  static class test3
  {
    public test3()
    {
    }

    static void check(TestHarness harness)
    {
      try
        {
          harness.check(test3.class.isInstance(test3.class.newInstance()));
        }
        catch (Throwable t)
        {
          harness.debug(t);
          harness.check(false);
        }
    }
  }

  public static class test4
  {
    test4()
    {
    }

    static void check(TestHarness harness)
    {
      try
        {
          harness.check(test4.class.isInstance(test4.class.newInstance()));
        }
        catch (Throwable t)
        {
          harness.debug(t);
          harness.check(false);
        }
    }
  }

  public static class test5
  {
    private test5()
    {
    }

    static void check(TestHarness harness)
    {
      try
        {
          harness.check(test5.class.isInstance(test5.class.newInstance()));
        }
        catch (Throwable t)
        {
          harness.debug(t);
          harness.check(false);
        }
    }
  }

  public static class test6
  {
    public test6() throws IOException
    {
      throw new IOException("hi bob");
    }
    
    static void check(TestHarness harness)
    {
      boolean ok = false;
      try
        {
    	  test6.class.newInstance();
        }
      catch (Throwable t)
        {
    	  harness.debug(t);
    	  ok = t instanceof IOException;
        }
      harness.check(ok);
    }
  }

  public void test(TestHarness harness)
  {
    test1.check(harness);
    test2.check(harness);
    test3.check(harness);
    test4.check(harness);
    test5.check(harness);

    checkSuccess(harness, test1.class);
    checkSuccess(harness, test2.class);
    checkSuccess(harness, test3.class);
    checkSuccess(harness, test4.class);
    // Just see to it that the following is legal.
    new test5();
    // If new test5() is legal, why should test5.class.newInstance()
    // throw IllegalAccessException?  The reason that it is different is
    // that 'new test5()' will call a compiler-generated accessor
    // constructor.  This accessor has package-private access and an
    // extra argument (to differentiate it from the user-written
    // constructor).
    checkFail(harness, test5.class);

    checkSuccess(harness, test1.inner.class);

    try
      {
        checkFail(harness, Class.forName("gnu.testlet.java.lang.Class.pkg.test1"));
        checkSuccess(harness, Class.forName("gnu.testlet.java.lang.Class.pkg.test2"));
        checkFail(harness, Class.forName("gnu.testlet.java.lang.Class.pkg.test3"));
        checkFail(harness, Class.forName("gnu.testlet.java.lang.Class.pkg.test4"));
      }
    catch (ClassNotFoundException x)
      {
        harness.debug(x);
        harness.fail("test configuration failure");
      }
    
    test6.check(harness);

    boolean thrown;
    // Interfaces cannot be instantiated
    try
      {
        Runnable.class.newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

    // Abstract classes cannot be instantiated
    try
      {
        Number.class.newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

    // Array classes cannot be instantiated
    try
      {
        new Object[1].getClass().newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

    // Primitive classes cannot be instantiated
    try
      {
        Byte.TYPE.newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

    // Void cannot be instantiated
    try
      {
        Void.TYPE.newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

    // No nullary constructor cannot be instantiated
    try
      {
        Integer.class.newInstance();
        thrown = false;
      }
    catch (IllegalAccessException iae)
      {
        thrown = false; // Wrong one
      }
    catch (InstantiationException ie)
      {
        thrown = true;
      }
    harness.check(thrown);

  }

  static void checkSuccess(TestHarness harness, Class c)
  {
    try
      {
        harness.check(c.isInstance(c.newInstance()));
      }
    catch (Throwable t)
      {
        harness.debug(t);
        harness.check(false);
      }
  }

  static void checkFail(TestHarness harness, Class c)
  {
    try
      {
        c.newInstance();
        harness.check(false);
      }
    catch (IllegalAccessException x)
      {
        harness.check(true);
      }
    catch (Throwable t)
      {
        harness.debug(t);
        harness.check(false);
      }
  }
}
