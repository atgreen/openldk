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

import java.lang.management.ClassLoadingMXBean;
import java.lang.management.ManagementFactory;

/**
 * Gets hold of a class loading management bean from the
 * @link{java.lang.management.ManagementFactory} and
 * test its security.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class ClassLoadingMXBeanTest
  implements Testlet
{
  
  private BadGuy bg = new BadGuy();

  public void test(TestHarness h)
  {
    try
      {
	Exception caught = null;
	ClassLoadingMXBean bean = ManagementFactory.getClassLoadingMXBean();

	bg.install();

	// Check getName
        caught = null;
        try
          {
            bean.setVerbose(true);
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "verbose");

      }
    finally
      {
	bg.uninstall();
      }
  }
  
}

