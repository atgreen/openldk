// Tags: JDK1.5

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

package gnu.testlet.java.util.logging.LoggingMXBean;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import gnu.testlet.java.util.logging.Logger.TestSecurityManager;

import java.util.logging.LoggingMXBean;
import java.util.logging.LogManager;

/**
 * Gets hold of a logging bean from the
 * @link{java.util.logging.LogManager} and
 * test its security.
 *
 * @author <a href="mailto:gnu_andrew@member.fsf.org">Andrew John Hughes</a>
 */
public class Test
  implements Testlet
{
  
  private TestSecurityManager tsm = new TestSecurityManager();

  public void test(TestHarness h)
  {
    try
      {
	Exception caught = null;
	LoggingMXBean bean = LogManager.getLoggingMXBean();

	tsm.install();

	// Check setLoggerLevel
        caught = null;
        try
          {
            bean.setLoggerLevel("global", null);
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException, "loggerLevel");
        try
          {
            bean.setLoggerLevel("NotALogger", null);
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof IllegalArgumentException, "loggerLevel");
        try
          {
            bean.setLoggerLevel(null, null);
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof NullPointerException, "loggerLevel");
      }
    finally
      {
	tsm.uninstall();
      }
  }
  
}

