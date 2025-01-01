// Tags: JDK1.2

// Copyright (C) 2005, 2006 Free Software Foundation, Inc.
// Written by Jeroen Frijters  <jeroen@frijters.net>

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

package gnu.testlet.java.lang.ClassLoader;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This test simulates a security attack dealing with the registering of a rogue
 * ClassLoader when it is not allowed. The detail of the potentiel problem is
 * described 
 * <a href="http://www.securingjava.com/chapter-five/chapter-five-8.html">here</a>.
 * Basically, it creates an incomplete ClassLoader (by throwing an exception
 * during the construction) and later uses the finalizer to retrieve the
 * instance and try to use this rogue instance. This test makes sure that any
 * method call then throws a SecurityException.
 * Running finalizers being not an exact science, some jvm will not run them
 * when System.runFinalization() is called hence not allowing the security
 * breach to be checked.
 * 
 * @author Jeroen Frijters <jeroen@frijters.net>
 */
public class initialize implements Testlet
{
  static class TestLoader extends ClassLoader
  {
    // The holder for the rogue TestLoader instance
    static TestLoader ref;
    
    // The method which simulates an exception to be thrown at construction time
    static ClassLoader throwException() { throw new Error(); }
    
    // The constructor which will fail to create a complete instance
    TestLoader() { super(throwException()); }
    
    // The finalizer which retrieves the partly created instance
    protected void finalize() { ref = this; }

    static void runTests(TestHarness harness) throws Exception
    {
      harness.checkPoint("loadClass");
      try
      {
        ref.loadClass("java.lang.Object");
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      try
      {
        ref.loadClass("java.lang.Object", false);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("findClass");
      try
      {
        ref.findClass("java.lang.Object");
        harness.check(false);
      }
      catch(ClassNotFoundException _)
      {
        harness.check(true);
      }

      harness.checkPoint("defineClass");
      try
      {
        ref.defineClass(new byte[0], 0, 0);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      try
      {
        ref.defineClass("Foo", new byte[0], 0, 0);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      try
      {
        ref.defineClass("Foo", new byte[0], 0, 0, null);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("resolveClass");
      try
      {
        ref.resolveClass(String.class);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("findSystemClass");
      try
      {
        ref.findSystemClass("java.lang.Object");
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("setSigners");
      try
      {
        ref.setSigners(String.class, new Object[0]);
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("findLoadedClass");
      try
      {
        ref.findLoadedClass("java.lang.Object");
        harness.check(false);
      }
      catch(SecurityException _)
      {
        harness.check(true);
      }

      harness.checkPoint("definePackage");
      try
      {
        ref.definePackage("Foo", "", "", "", "", "", "", null);
        harness.check(false);
      }
      catch(NullPointerException _)
      {
        harness.check(true);
      }

      try
      {
        ref.getPackage("Foo");
        harness.check(false);
      }
      catch(NullPointerException _)
      {
        harness.check(true);
      }

      try
      {
        ref.getPackages();
        harness.check(false);
      }
      catch(NullPointerException _)
      {
        harness.check(true);
      }

    }
  }

  public void test(TestHarness harness)
  {
    // Creates a garbage collectable rogue TestLoader instance
    try { new TestLoader(); } catch(Error x) {}
    
    // Hints at the vm that running finalizers now would be a good idea
    System.gc();
    System.runFinalization();
    
    // Checks that TestLoader.finalize retrieved the partly created instance,
    // and if so, tests it
    if (TestLoader.ref == null)
      harness.debug("Unable to obtain finalized ClassLoader instance");
    else
    {
      try
      {
        TestLoader.runTests(harness);
      }
      catch(Exception x)
      {
        harness.debug(x);
        harness.check(false);
      }
    }
  }
}
