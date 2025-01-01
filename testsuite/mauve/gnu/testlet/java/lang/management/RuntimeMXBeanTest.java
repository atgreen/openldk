// Tags: JDK1.5
// Uses: BadGuy

// Copyright (C) 2006 Andrew John Hughes <gnu_andrew@member.fsf.org>

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

package gnu.testlet.java.lang.management;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;

/**
 * Gets hold of a runtime management bean from the
 * @link{java.lang.management.ManagementFactory} and
 * test its security.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class RuntimeMXBeanTest
  implements Testlet
{
  
  private BadGuy bg = new BadGuy();

  public void test(TestHarness h)
  {
    try
      {
	Exception caught = null;
	RuntimeMXBean bean = ManagementFactory.getRuntimeMXBean();

	bg.install();

	// Check getVmName
        caught = null;
        try
          {
            bean.getVmName();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "vmName");

	// Check getVmVendor
        caught = null;
        try
          {
            bean.getVmVendor();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "vmVendor");

	// Check getVmVersion
        caught = null;
        try
          {
            bean.getVmVersion();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "vmVersion");

	// Check getSpecName
        caught = null;
        try
          {
            bean.getSpecName();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "specName");

	// Check getSpecVendor
        caught = null;
        try
          {
            bean.getSpecVendor();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "specVendor");

	// Check getSpecVersion
        caught = null;
        try
          {
            bean.getSpecVersion();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "specVersion");

	// Check getClassPath
        caught = null;
        try
          {
            bean.getClassPath();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "classPath");

	// Check getLibraryPath
        caught = null;
        try
          {
            bean.getLibraryPath();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "libraryPath");

	// Check getBootClassPath
        caught = null;
        try
          {
            bean.getBootClassPath();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
	if (bean.isBootClassPathSupported())
	  h.check(caught instanceof SecurityException, "bootClassPath");
	else
	  h.check(caught instanceof UnsupportedOperationException, "bootClassPath");

	// Check getInputArguments
        caught = null;
        try
          {
            bean.getInputArguments();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "inputArguments");

	// Check getSystemProperties
        caught = null;
        try
          {
            bean.getSystemProperties();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "systemProperties");

      }
    finally
      {
	bg.uninstall();
      }
  }
  
}

