// Tags: JDK1.1

// Copyright (C) 2005 Free Software Foundation, Inc.
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

public class findLoadedClass extends ClassLoader implements Testlet
{
  // This represents the class:
  //   class Triv extends java.util.Hashtable {}
  private static byte[] trivialClassDef = {
    (byte)0xCA, (byte)0xFE, (byte)0xBA, (byte)0xBE, (byte)0x00, (byte)0x03,
    (byte)0x00, (byte)0x2D, (byte)0x00, (byte)0x0F, (byte)0x07, (byte)0x00,
    (byte)0x0C, (byte)0x07, (byte)0x00, (byte)0x0E, (byte)0x0A, (byte)0x00,
    (byte)0x02, (byte)0x00, (byte)0x04, (byte)0x0C, (byte)0x00, (byte)0x06,
    (byte)0x00, (byte)0x05, (byte)0x01, (byte)0x00, (byte)0x03, (byte)0x28,
    (byte)0x29, (byte)0x56, (byte)0x01, (byte)0x00, (byte)0x06, (byte)0x3C,
    (byte)0x69, (byte)0x6E, (byte)0x69, (byte)0x74, (byte)0x3E, (byte)0x01,
    (byte)0x00, (byte)0x04, (byte)0x43, (byte)0x6F, (byte)0x64, (byte)0x65,
    (byte)0x01, (byte)0x00, (byte)0x0D, (byte)0x43, (byte)0x6F, (byte)0x6E,
    (byte)0x73, (byte)0x74, (byte)0x61, (byte)0x6E, (byte)0x74, (byte)0x56,
    (byte)0x61, (byte)0x6C, (byte)0x75, (byte)0x65, (byte)0x01, (byte)0x00,
    (byte)0x0A, (byte)0x45, (byte)0x78, (byte)0x63, (byte)0x65, (byte)0x70,
    (byte)0x74, (byte)0x69, (byte)0x6F, (byte)0x6E, (byte)0x73, (byte)0x01,
    (byte)0x00, (byte)0x0E, (byte)0x4C, (byte)0x6F, (byte)0x63, (byte)0x61,
    (byte)0x6C, (byte)0x56, (byte)0x61, (byte)0x72, (byte)0x69, (byte)0x61,
    (byte)0x62, (byte)0x6C, (byte)0x65, (byte)0x73, (byte)0x01, (byte)0x00,
    (byte)0x0A, (byte)0x53, (byte)0x6F, (byte)0x75, (byte)0x72, (byte)0x63,
    (byte)0x65, (byte)0x46, (byte)0x69, (byte)0x6C, (byte)0x65, (byte)0x01,
    (byte)0x00, (byte)0x04, (byte)0x54, (byte)0x72, (byte)0x69, (byte)0x76,
    (byte)0x01, (byte)0x00, (byte)0x09, (byte)0x54, (byte)0x72, (byte)0x69,
    (byte)0x76, (byte)0x2E, (byte)0x6A, (byte)0x61, (byte)0x76, (byte)0x61,
    (byte)0x01, (byte)0x00, (byte)0x13, (byte)0x6A, (byte)0x61, (byte)0x76,
    (byte)0x61, (byte)0x2F, (byte)0x75, (byte)0x74, (byte)0x69, (byte)0x6C,
    (byte)0x2F, (byte)0x48, (byte)0x61, (byte)0x73, (byte)0x68, (byte)0x74,
    (byte)0x61, (byte)0x62, (byte)0x6C, (byte)0x65, (byte)0x00, (byte)0x00,
    (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x02, (byte)0x00, (byte)0x00,
    (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00,
    (byte)0x00, (byte)0x06, (byte)0x00, (byte)0x05, (byte)0x00, (byte)0x01,
    (byte)0x00, (byte)0x07, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x11,
    (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x01, (byte)0x00, (byte)0x00,
    (byte)0x00, (byte)0x05, (byte)0x2A, (byte)0xB7, (byte)0x00, (byte)0x03,
    (byte)0xB1, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00, (byte)0x00,
    (byte)0x01, (byte)0x00, (byte)0x0B, (byte)0x00, (byte)0x00, (byte)0x00,
    (byte)0x02, (byte)0x00, (byte)0x0D
  };

  private boolean broken;

  public findLoadedClass()
  {
  }

  protected synchronized Class loadClass(String name, boolean resolve)
    throws ClassNotFoundException
  {
    if (broken)
        throw new ClassNotFoundException();
    else
        return super.loadClass(name, resolve);
  }

  private findLoadedClass(ClassLoader parent)
  {
    super(parent);
  }

  public void test(TestHarness harness)
  {
    defineClass("Triv", trivialClassDef, 0, trivialClassDef.length);

    // defineClass should have registered the class
    harness.checkPoint("defineClass should register");
    checkLoaded(harness, this, "Triv");

    // make sure that the VM registers the initiating class loader
    harness.checkPoint("VM should register");
    checkLoaded(harness, this, "java.util.Hashtable");

    // types that weren't loaded shouldn't be visible
    harness.checkPoint("premature");
    harness.check(findLoadedClass("java.lang.Object") == null);

    // Class.forName() should register the initiating loader.
    harness.checkPoint("Class.forName");
    try
    {
        Class.forName("java.lang.Object", false, this);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
    }
    checkLoaded(harness, this, "java.lang.Object");

    // The above should also apply to arrays
    // Note that on Sun JDK 1.4 (not on 1.5), loading the component type
    // also make the array type visible, so we don't test that the array
    // is not visible at this point.
    try
    {
        Class.forName("[Ljava.lang.Object;", false, this);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
    }
    checkLoaded(harness, this, "[Ljava.lang.Object;");

    // Loading an array type, makes available the ultimate component type
    harness.checkPoint("array implies component type");
    harness.check(findLoadedClass("java.util.Vector") == null);    
    try
    {
        Class.forName("[[Ljava.util.Vector;", false, this);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
    }
    checkLoaded(harness, this, "java.util.Vector");

    // After loading a class thru a parent, we shouldn't be able to define it.
    harness.checkPoint("no redefine");
    findLoadedClass cl = new findLoadedClass(this);
    harness.check(cl.findLoadedClass("Triv") == null);
    try
    {
        Class.forName("Triv", false, cl);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
        throw new Error(x);
    }
    checkLoaded(harness, cl, "Triv");
    try
    {
        cl.defineClass("Triv", trivialClassDef, 0, trivialClassDef.length);
        harness.check(false);
    }
    catch(LinkageError _)
    {
        harness.check(true);
    }

    // Check multi level trickery
    harness.checkPoint("multi level");
    findLoadedClass grandParent = new findLoadedClass();
    grandParent.defineClass("Triv", trivialClassDef, 0, trivialClassDef.length);
    findLoadedClass parent = new findLoadedClass(grandParent);
    findLoadedClass child = new findLoadedClass(parent);
    try
    {
        Class.forName("Triv", false, child);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
        throw new Error(x);
    }
    try
    {
        parent.defineClass("Triv", trivialClassDef, 0, trivialClassDef.length);
        harness.check(true);
    }
    catch(LinkageError x)
    {
        harness.debug(x);
        harness.check(false);
    }
    try
    {
        Class c = Class.forName("Triv", false, child);
        harness.check(c.getClassLoader() == grandParent);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
        harness.check(false);
    }
    catch(LinkageError x)
    {
        harness.debug(x);
        harness.check(false);
    }
    // Even if a class loader is broken, Class.forName() should continue
    // to work.
    child.broken = true;
    try
    {
        Class c = Class.forName("Triv", false, child);
        harness.check(c.getClassLoader() == grandParent);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
        harness.check(false);
    }
    catch(LinkageError x)
    {
        harness.debug(x);
        harness.check(false);
    }

    // The VM should also look in the loaded classes cache, before calling loadClass()
    harness.checkPoint("VM consults cache");
    findLoadedClass newLoader = new findLoadedClass();
    try
    {
        Class.forName("java.util.Hashtable", false, newLoader);
    }
    catch(ClassNotFoundException x)
    {
        harness.debug(x);
        throw new Error(x);
    }
    newLoader.broken = true;
    newLoader.defineClass("Triv", trivialClassDef, 0, trivialClassDef.length);
  }

  private void checkLoaded(TestHarness harness, findLoadedClass cl, String name)
  {
    Class c = cl.findLoadedClass(name);
    harness.check(c != null && c.getName().equals(name));
  }
}
