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

package gnu.testlet.java.lang.System;

import java.security.Permission;
import java.util.Properties;
import java.util.PropertyPermission;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import gnu.testlet.TestSecurityManager;

public class security implements Testlet
{
  public void test(TestHarness harness)
  {
    String not_a_property = "java.monkey.flump";
    try {
      harness.checkPoint("setup");

      String library_name = "blah";
      String library_path = "/path/to/libnothing.so";

      String a_variable = "PATH";
      String not_a_variable = "PITH";
      harness.check(System.getenv(a_variable) != null);
      harness.check(System.getenv(not_a_variable) == null);
      
      Properties properties = System.getProperties();

      String a_property = "java.vm.name";
      harness.check(properties.containsKey(a_property));
      harness.check(!properties.containsKey(not_a_property));
      
      //exitVM.0 or set compare style to implies
      Permission[] exitVM = new Permission[] {
	new RuntimePermission("exitVM.0")};

      Permission[] loadLibrary_name = new Permission[] {
	new RuntimePermission("loadLibrary." + library_name)};
      Permission[] loadLibrary_path = new Permission[] {
	new RuntimePermission("loadLibrary." + library_path)};

      Permission[] readVariable = new Permission[] {
	new RuntimePermission("getenv." + a_variable)};
      Permission[] readNonVariable = new Permission[] {
	new RuntimePermission("getenv." + not_a_variable)};

      Permission[] readWriteAllProperties = new Permission[] {
	new PropertyPermission("*", "read,write")};

      Permission[] readProperty = new Permission[] {
	new PropertyPermission(a_property, "read")};
      Permission[] readNonProperty = new Permission[] {
	new PropertyPermission(not_a_property, "read")};

      Permission[] setIO = new Permission[] {
	new RuntimePermission("setIO")};
      
      Permission[] writeProperty = new Permission[] {
	new PropertyPermission(a_property, "write")};
      Permission[] writeNonProperty = new Permission[] {
	new PropertyPermission(not_a_property, "write")};

      Permission[] setSecurityManager = new Permission[] {
	new RuntimePermission("setSecurityManager")};
      
      TestSecurityManager sm = new TestSecurityManager(harness);
      try {
	sm.install();

	// throwpoint: java.lang.System-exit
	harness.checkPoint("exit");
	try {
	  sm.prepareHaltingChecks(exitVM);
	  System.exit(0);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
	} 
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	
	// throwpoint: java.lang.System-runFinalizersOnExit
	harness.checkPoint("runFinalizersOnExit");
	try {
	  sm.prepareChecks(exitVM);
	  System.runFinalizersOnExit(false);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-load
	harness.checkPoint("load");
	try {
	  sm.prepareHaltingChecks(loadLibrary_name);
	  System.load(library_name);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
	} 
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-loadLibrary
	harness.checkPoint("loadLibrary");
	try {
	  sm.prepareHaltingChecks(loadLibrary_path);
	  System.loadLibrary(library_path);
	  harness.check(false);	  
	}
	catch (TestSecurityManager.SuccessException ex) {
	  harness.check(true);
	} 
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: TODO: java.lang.System-getenv()

	// throwpoint: java.lang.System-getenv(String)
	harness.checkPoint("getenv(String)");
	try {
	  sm.prepareChecks(readVariable);
	  System.getenv(a_variable);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(readNonVariable);
	  System.getenv(not_a_variable);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-getProperties
	harness.checkPoint("getProperties");
	try {
	  sm.prepareChecks(readWriteAllProperties);
	  System.getProperties();
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-setProperties
	harness.checkPoint("setProperties");
	try {
	  sm.prepareChecks(readWriteAllProperties);
	  System.setProperties(properties);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-getProperty(String)
	harness.checkPoint("getProperty(String)");
	try {
	  sm.prepareChecks(readProperty);
	  System.getProperty(a_property);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(readNonProperty);
	  System.getProperty(not_a_property);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-getProperty(String, String)
	harness.checkPoint("getProperty(String, String)");
	try {
	  sm.prepareChecks(readProperty);
	  System.getProperty(a_property, "quadrant");
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(readNonProperty);
	  System.getProperty(not_a_property, "blade");
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-setIn
	harness.checkPoint("setIn");
	try {
	  sm.prepareChecks(setIO);
	  System.setIn(System.in);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-setOut
	harness.checkPoint("setOut");
	try {
	  sm.prepareChecks(setIO);
	  System.setOut(System.out);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-setErr
	harness.checkPoint("setErr");
	try {
	  sm.prepareChecks(setIO);
	  System.setErr(System.err);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: java.lang.System-setProperty
	harness.checkPoint("setProperty");
	try {
	  sm.prepareChecks(writeProperty);
	  System.setProperty(a_property, properties.getProperty(a_property));
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
	try {
	  sm.prepareChecks(writeNonProperty);
	  System.setProperty(not_a_property, "hello mum");
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}

	// throwpoint: TODO: java.lang.System-clearProperty

	// throwpoint: java.lang.System-setSecurityManager
	harness.checkPoint("setSecurityManager");
	try {
	  sm.prepareChecks(setSecurityManager);
	  System.setSecurityManager(sm);
	  sm.checkAllChecked();
	}
	catch (SecurityException ex) {
	  harness.debug(ex);
	  harness.check(false, "unexpected check");
	}
      }
      finally {
	sm.uninstall();
        System.clearProperty(not_a_property);
      }
    }
    catch (Exception ex) {
      harness.debug(ex);
      harness.check(false, "Unexpected exception");
    }
  }
}
