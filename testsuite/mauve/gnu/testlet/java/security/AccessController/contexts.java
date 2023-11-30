// Copyright (C) 2006, 2007 Red Hat, Inc.
// Written by Gary Benson <gbenson@redhat.com>

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

// Tags: JDK1.2

package gnu.testlet.java.security.AccessController;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.FilePermission;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.AccessController;
import java.security.AccessControlContext;
import java.security.Permission;
import java.security.PermissionCollection;
import java.security.PrivilegedAction;
import java.security.ProtectionDomain;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

// In this test we load three different instances of ourself from
// three different jarfiles with three different classloaders.  Each
// classloader has a different protection domain in which that
// classloader's jarfile is readable.  All kinds of context-hopping is
// performed, and we infer which protection domains are in our stack
// by seeing which jarfile read permissions we can see.

public class contexts implements Testlet
{
  public void test(TestHarness harness)
  {
    // The bits where we access protection domains is Classpath-specific.
    if (System.getProperty("gnu.classpath.version") == null)
      return;

    File jars[] = new File[] {null, null, null};
    try {
      harness.checkPoint("setup");

      // Make jarfiles containing this class and its dependencies
      String base =
          new File(harness.getTempDirectory(), "ac").getCanonicalPath();

      jars[0] = new File(base + "1.jar");
      JarOutputStream jos = new JarOutputStream(new FileOutputStream(jars[0]));
      copyClass(harness.getBuildDirectory(), jos, getClass());
      copyClass(harness.getBuildDirectory(), jos, TestHarness.class);
      jos.close();

      for (int i = 1; i < jars.length; i++) {
	jars[i] = new File(base + (i + 1) + ".jar");
	copyFile(jars[0], jars[i]);
      }

      // Create instances of ourself loaded from different loaders
      TestObject testObjects[] = new TestObject[jars.length];
      for (int i = 0; i < jars.length; i++) {
	Class testClass = new URLClassLoader(new URL[] {
	  jars[i].toURL()}, null).loadClass(getClass().getName());
	harness.check(
	  getClass().getClassLoader() != testClass.getClassLoader());
	Constructor c = testClass.getConstructor(new Class[] {String.class});
	testObjects[i] = new TestObject(c.newInstance(new Object[] {base}));
      }

      // Run the tests
      test(harness, testObjects);
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
    finally {
      for (int i = 0; i < jars.length; i++) {
	if (jars[i].exists())
	  jars[i].delete();
      }
    }
  }

  // Copy a classfile and its dependencies into a jarfile
  private static void copyClass(String srcdir, JarOutputStream jos, Class cls)
    throws Exception
  {
    File root = new File(srcdir, cls.getName().replace(".", File.separator));
    final String rootpath = root.getPath();
    int chop = srcdir.length() + File.separator.length();

    File dir = root.getParentFile();
    if (dir.isDirectory()) {
      File[] files = dir.listFiles(new FileFilter() {
	public boolean accept(File file) {
	  String path = file.getPath();
	  if (path.endsWith(".class")) {
	    path = path.substring(0, path.length() - 6);
	    if (path.equals(rootpath))
	      return true;
	    if (path.startsWith(rootpath + "$"))
	      return true;
	  }
	  return false;
	}
      });
      for (int i = 0; i < files.length; i++) {
	byte[] bytes = new byte[(int) files[i].length()];
	FileInputStream fis = new FileInputStream(files[i]);
	fis.read(bytes);
	fis.close();

	jos.putNextEntry(new JarEntry(files[i].getPath().substring(chop)));
	jos.write(bytes, 0, bytes.length);
      }
    }

    Class superclass = cls.getSuperclass();
    if (superclass != null)
      copyClass(srcdir, jos, superclass);
    Class[] interfaces = cls.getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
      copyClass(srcdir, jos, interfaces[i]);
  }

  // Make a copy of a file
  private static void copyFile(File src, File dst) throws Exception
  {
    byte[] bytes = new byte[(int) src.length()];
    FileInputStream fis = new FileInputStream(src);
    fis.read(bytes);
    fis.close();

    FileOutputStream fos = new FileOutputStream(dst);
    fos.write(bytes);
    fos.close();
  }

  // Constructor for the main object that Mauve creates
  public contexts()
  {
  }
  
  // Constructor for the sub-objects that the main object creates
  private String base = null;
  public contexts(String base)
  {
    this.base = base;
  }
  
  // Wrapper to hide the pain of reflection
  private static class TestObject
  {
    private Object object;

    public TestObject(Object object)
    {
      this.object = object;
    }

    public String[] listJarsOf(TestObject other) throws Exception
    {
      Method method = object.getClass().getMethod(
	"listJarsOf", new Class[] {Object.class});
      return (String[]) method.invoke(object, new Object[] {other.object});
    }

    public String[] callListJarsOf(TestObject caller, TestObject callee)
      throws Exception
    {
      Method method = object.getClass().getMethod(
	"callListJarsOf", new Class[] {Object.class, Object.class});
      return (String[]) method.invoke(
	object, new Object[] {caller.object, callee.object});
    }

    public String[] callPrivilegedListJarsOf(
      TestObject caller, TestObject callee) throws Exception
    {
      Method method = object.getClass().getMethod(
	"callPrivilegedListJarsOf", new Class[] {Object.class, Object.class});
      return (String[]) method.invoke(
	object, new Object[] {caller.object, callee.object});
    }
  }

  public String[] listJarsOf(Object object) throws Exception
  {
    Method method = object.getClass().getMethod("listJars", new Class[0]);
    return (String[]) method.invoke(object, new Object[0]);
  }

  public String[] callListJarsOf(Object caller, Object callee)
    throws Exception
  {
    Method method = caller.getClass().getMethod(
      "listJarsOf", new Class[] {Object.class});
    return (String[]) method.invoke(caller, new Object[] {callee});
  }

  public String[] callPrivilegedListJarsOf(Object caller, Object callee)
    throws Exception
  {
    Method method = caller.getClass().getMethod(
      "privilegedListJarsOf", new Class[] {Object.class});
    return (String[]) method.invoke(caller, new Object[] {callee});
  }

  public String[] privilegedListJarsOf(final Object object) throws Exception
  {
    final Method method = object.getClass().getMethod(
      "listJars", new Class[0]);
    return (String[]) AccessController.doPrivileged(new PrivilegedAction() {
      public Object run() {
	try {
	  return method.invoke(object, new Object[0]);
	}
	catch (Exception e) {
	  return e;
	}
      }
    });
  }

  public String[] listJars() throws Exception
  {
    AccessControlContext ctx = AccessController.getContext();
    // XXX start of classpath-specific code
    Field field = ctx.getClass().getDeclaredField("protectionDomains");
    field.setAccessible(true);
    ProtectionDomain[] domains = (ProtectionDomain[]) field.get(ctx);
    // XXX end of classpath-specific code

    LinkedList jars = new LinkedList();
    for (int i = 0; i < domains.length; i++) {
      PermissionCollection perms = domains[i].getPermissions();
      for (Enumeration e = perms.elements(); e.hasMoreElements() ;) {
	Permission p = (Permission) e.nextElement();
	if (!(p instanceof FilePermission))
	  continue;
	String path = p.getName();
	if (path.length() == base.length() + 5
	    && path.startsWith(base)
	    && Character.isDigit(path.charAt(base.length()))
	    && path.endsWith(".jar"))
	  jars.add(path);
      }
    }
    return (String[]) jars.toArray(new String[jars.size()]);
  }
  
  // Perform the tests
  private static void test(TestHarness harness, TestObject[] objects)
    throws Exception
  {
    // Each object should see only its own protection domain
    harness.checkPoint("self-listing");

    String[] jars = new String[objects.length];
    for (int i = 0; i < objects.length; i++) {
      String[] result = objects[i].listJarsOf(objects[i]);
      harness.check(result.length == 1);
      jars[i] = result[0];
    }
    for (int i = 0; i < objects.length; i++) {
      for (int j = i + 1; j < objects.length; j++)
	harness.check(!jars[i].equals(jars[j]));
    }
    
    // When one object calls another both objects' protection domains
    // should be present.
    harness.checkPoint("straight other-listing");

    boolean[] seen = new boolean[jars.length];
    String[] result = objects[0].listJarsOf(objects[1]);
    harness.check(result.length == 2);
    for (int i = 0; i < seen.length; i++) {
      seen[i] = false;
      for (int j = 0; j < result.length; j++) {
	if (result[j].equals(jars[i])) {
	  harness.check(!seen[i]);
	  seen[i] = true;
	}
      }
    }
    harness.check(seen[0] && seen[1] && !seen[2]);

    // When one object calls another that calls another all three
    // objects' protection domains should be present.
    harness.checkPoint("straight other-other-listing");

    result = objects[0].callListJarsOf(objects[1], objects[2]);
    harness.check(result.length == 3);
    for (int i = 0; i < seen.length; i++) {
      seen[i] = false;
      for (int j = 0; j < result.length; j++) {
	if (result[j].equals(jars[i])) {
	  harness.check(!seen[i]);
	  seen[i] = true;
	}
      }
    }
    harness.check(seen[0] && seen[1] && seen[2]);

    // When one object calls another that uses doPrivileged to call
    // a third then the first object's protection domain should not
    // be present.
    harness.checkPoint("privileged other-other-listing");

    result = objects[0].callPrivilegedListJarsOf(objects[1], objects[2]);
    harness.check(result.length == 2);
    for (int i = 0; i < seen.length; i++) {
      seen[i] = false;
      for (int j = 0; j < result.length; j++) {
	if (result[j].equals(jars[i])) {
	  harness.check(!seen[i]);
	  seen[i] = true;
	}
      }
    }
    harness.check(!seen[0] && seen[1] && seen[2]);
  }
}
