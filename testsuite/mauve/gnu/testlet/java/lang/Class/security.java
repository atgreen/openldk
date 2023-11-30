// Copyright (C) 2005, 2006, 2007 Red Hat, Inc.
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

package gnu.testlet.java.lang.Class;

import java.io.File;
import java.lang.reflect.Member;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.Permission;
import java.security.Security;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    try {
      harness.checkPoint("setup");

      // we need a class with a different loader for most of the
      // checks to occur.
      Class testClass = new URLClassLoader(new URL[] {
	new File(harness.getBuildDirectory()).toURL()}, null).loadClass(
	  getClass().getName());
      harness.check(getClass().getClassLoader() != testClass.getClassLoader());

      // Make sure everything's fully resolved, or we'll be loading
      // classes during tests and the extra checks will make us fail.
      testClass.getDeclaredClasses();
      testClass.getDeclaredMethods();

      // we need to restrict access to some packages for some of the
      // checks to occur.
      String oldrestrictions = Security.getProperty("package.access");
      Security.setProperty(
	"package.access",
	"gnu.testlet.java.lang.Class.");

      try {
	Permission[] noChecks = new Permission[] { };

	Permission[] getClassLoader = new Permission[] {
	  new RuntimePermission("getClassLoader")};

	Permission[] accessDeclaredMembers = new Permission[] {
	  new RuntimePermission("accessDeclaredMembers"),
	  new RuntimePermission(
	    "accessClassInPackage.gnu.testlet.java.lang.Class")};
	
	Permission[] accessPublicMembers = new Permission[] {
	  new RuntimePermission(TestSecurityManager3.publicPerm),
	  new RuntimePermission(
	    "accessClassInPackage.gnu.testlet.java.lang.Class")};

	Permission[] getProtectionDomain = new Permission[] {
	  new RuntimePermission("getProtectionDomain")};
	
	TestSecurityManager sm = new TestSecurityManager(harness);
	try {
	  sm.install();

	  // throwpoint: java.lang.Class-forName
	  harness.checkPoint("forName");
	  try {
	    sm.prepareChecks(getClassLoader);
	    Class.forName("java.lang.Class", false, null);
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // throwpoint: java.lang.Class-getClassLoader
	  harness.checkPoint("getClassLoader");
	  try {
	    sm.prepareChecks(getClassLoader);
	    testClass.getClassLoader();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // If it is a bootstrap class, there is a null classloader
	  // and no checks are necessary.
	  try {
	    sm.prepareChecks(noChecks);
	    Thread.class.getClassLoader();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }

	  // getDeclaredMember checks
	  getMemberChecks(harness, sm, testClass, true, accessDeclaredMembers);
	  
	  // throwpoint: java.lang.Class-getProtectionDomain
	  harness.checkPoint("getProtectionDomain");
	  try {
	    sm.prepareChecks(getProtectionDomain);
	    testClass.getProtectionDomain();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	  try {
	    sm.prepareChecks(getProtectionDomain);
	    getClass().getProtectionDomain();
	    sm.checkAllChecked();
	  }
	  catch (SecurityException ex) {
	    harness.debug(ex);
	    harness.check(false, "unexpected check");
	  }
	}
	finally {
	  sm.uninstall();
	}

	// these tests need a modified security manager
	sm = new TestSecurityManager3(harness);
	try {
	  sm.install();

	  // getMember checks
	  getMemberChecks(harness, sm, testClass, false, accessPublicMembers);
	}
	finally {
	  sm.uninstall();
	}
      }
      finally {
        if (oldrestrictions != null)
          {
            Security.setProperty("package.access", oldrestrictions);
          }
      }
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }

  private void getMemberChecks(TestHarness harness, TestSecurityManager sm,
			       Class testClass, boolean declared,
			       Permission[] mustCheck)
  {
    int level;
    
    // throwpoint: java.lang.Class-getClasses
    // throwpoint: java.lang.Class-getDeclaredClasses
    if (declared)
      harness.checkPoint("getDeclaredClasses");
    else
      harness.checkPoint("getClasses"); 
    try {
      sm.prepareChecks(mustCheck);
      if (declared)
	testClass.getDeclaredClasses();
      else
	testClass.getClasses();
      sm.checkAllChecked();
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }

    // throwpoint: java.lang.Class-getFields
    // throwpoint: java.lang.Class-getDeclaredFields
    if (declared)
      harness.checkPoint("getDeclaredFields");
    else
      harness.checkPoint("getFields");
    try {
      sm.prepareChecks(mustCheck);
      if (declared)
	testClass.getDeclaredFields();
      else
	testClass.getFields();
      sm.checkAllChecked();
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }

    // throwpoint: java.lang.Class-getMethods
    // throwpoint: java.lang.Class-getDeclaredMethods
    if (declared)
      harness.checkPoint("getDeclaredMethods");
    else
      harness.checkPoint("getMethods");
    try {
      sm.prepareChecks(mustCheck);
      if (declared)
	testClass.getDeclaredMethods();
      else
	testClass.getMethods();
      sm.checkAllChecked();
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }

    // throwpoint: java.lang.Class-getConstructors
    // throwpoint: java.lang.Class-getDeclaredConstructors
    if (declared)
      harness.checkPoint("getDeclaredConstructors");
    else
      harness.checkPoint("getConstructors");
    try {
      sm.prepareChecks(mustCheck);
      if (declared)
	testClass.getDeclaredConstructors();
      else
	testClass.getConstructors();
      sm.checkAllChecked();
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }

    // throwpoint: java.lang.Class-getField
    // throwpoint: java.lang.Class-getDeclaredField
    if (declared) {
      harness.checkPoint("getDeclaredField");
      level = 0;
    }
    else {
      harness.checkPoint("getField");
      level = 3;
    }
    try {
      for (int i = 0; i < modifiers.length; i++) {
	for (int j = 0; j < 5; j++) {
	  sm.prepareChecks(mustCheck);
	  boolean exists;
	  try {
	    String name = modifiers[i] + "field" + j;
	    if (declared)
	      testClass.getDeclaredField(name);
	    else
	      testClass.getField(name);
	    exists = true;
	  }
	  catch (NoSuchFieldException e) {
	    exists = false;
	  }
	  sm.checkAllChecked();
	  harness.check(exists == (j > level));
	}
      }
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }
    
    // throwpoint: java.lang.Class-getMethod
    // throwpoint: java.lang.Class-getDeclaredMethod
    if (declared)
      harness.checkPoint("getDeclaredMethod");
    else
      harness.checkPoint("getMethod");
    try {
      for (int i = 0; i < modifiers.length; i++) {
	for (int j = 0; j < 5; j++) {
	  for (int k = 0; k < methodtypes.length; k++) {
	    sm.prepareChecks(mustCheck);
	    boolean exists;
	    try {
	      String name = modifiers[i] + "method" + j;
	      if (declared)
		testClass.getDeclaredMethod(name, methodtypes[k]);
	      else
		testClass.getMethod(name, methodtypes[k]);
	      exists = true;
	    }
	    catch (NoSuchMethodException e) {
	      exists = false;
	    }
	    sm.checkAllChecked();
	    harness.check(exists == (j > level && k > 0));
	  }
	}
      }
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }

    // throwpoint: java.lang.Class-getConstructor
    // throwpoint: java.lang.Class-getDeclaredConstructor
    if (declared) {
      harness.checkPoint("getDeclaredConstructor");
      level = 0;
    }
    else {
      harness.checkPoint("getConstructor");
      level = 6;
    }
    try {
      for (int i = 0; i < constructortypes.length; i++) {
	sm.prepareChecks(mustCheck);
	boolean exists;
	try {
	  if (declared)
	    testClass.getDeclaredConstructor(constructortypes[i]);
	  else
	    testClass.getConstructor(constructortypes[i]);
	  exists = true;
	}
	catch (NoSuchMethodException e) {
	  exists = false;
	}
	sm.checkAllChecked();
	harness.check(exists == (i > level));
      }
    }
    catch (SecurityException ex) {
      harness.debug(ex);
      harness.check(false, "unexpected check");
    }
  }

  // Data tables for get{,Declared}{Field,Method,Constructor} checks
  private static String[] modifiers = new String[] {"", "static"};
  private static Class[][] methodtypes = new Class[][] {
    new Class[] {short.class},
    new Class[] {},
    new Class[] {int.class},
    new Class[] {String.class, boolean.class}};
  private static Class[][] constructortypes = new Class[][] {
    new Class[] {short.class},
    new Class[] {int.class},
    new Class[] {String.class, int.class},
    new Class[] {int.class, int.class},
    new Class[] {byte.class},
    new Class[] {String.class},
    new Class[] {int.class, String.class},
    new Class[] {int.class, int.class, int.class},
    new Class[] {}};

  // Fields for getField and getDeclaredField checks
  private boolean field1;
  byte field2;
  protected int field3;
  public String field4;

  private static boolean staticfield1;
  static byte staticfield2;
  protected static int staticfield3;
  public static String staticfield4;

  // Methods for getMethod and getDeclaredMethod checks
  private void method1() {}
  private void method1(int a) {}
  private void method1(String b, boolean c) {}
  char method2() { return 'a'; }
  char method2(int a) { return 'b'; }
  char method2(String b, boolean c) { return '1'; }
  protected String method3() { return "x0x"; }
  protected String method3(int a) { return "y"; }
  protected String method3(String b, boolean c) { return "z"; }
  public int method4() { return 1; }
  public int method4(int a) { return 0; }
  public int method4(String b, boolean c) { return -5; }

  private static void staticmethod1() {}
  private static void staticmethod1(int a) {}
  private static void staticmethod1(String b, boolean c) {}
  static char staticmethod2() { return 'a'; }
  static char staticmethod2(int a) { return 'b'; }
  static char staticmethod2(String b, boolean c) { return '1'; }
  protected static String staticmethod3() { return "x0x"; }
  protected static String staticmethod3(int a) { return "y"; }
  protected static String staticmethod3(String b, boolean c) { return "z"; }
  public static int staticmethod4() { return 1; }
  public static int staticmethod4(int a) { return 0; }
  public static int staticmethod4(String b, boolean c) { return -5; }

  // Constructors for getConstructor and getDeclaredConstructor checks
  private security(int a) {}
  private security(String a, int b) {}
  security(int a, int b) {}
  security(byte a) {}
  protected security(String a) {}
  protected security(int a, String b) {}
  public security(int a, int b, int c) {}
  public security() {}

  // The default implementation of SecurityManager.checkMemberAccess()
  // checks no permissions if memberType is Member.PUBLIC.  This class
  // changes this, allowing us to test get{Field,Method,Constructor}.
  // No other tests should use this class.
  private static class TestSecurityManager3 extends TestSecurityManager
  {
    TestSecurityManager3(TestHarness harness)
    {
      super(harness);
    }
    
    static String publicPerm = "gnuAccessPublicMembers";

    public void checkMemberAccess(Class c, int memberType)
    {
      if (c == null)
	throw new NullPointerException();
      if (memberType == Member.PUBLIC)
	checkPermission(new RuntimePermission(publicPerm));
    }
  }

  // Silence compiler warnings
  public void shutUp()
  {
    field1 = staticfield1 = false;
    new security(5).method1();
    new security("hello", 5).method1(5);
    method1("4", field1);
    staticmethod1();
    staticmethod1(5);
    staticmethod1("4", staticfield1);
  }
}
