// Tags: JDK1.4
// Uses: TestLogger TestResourceBundle TestSecurityManager

// Copyright (C) 2004 Sascha Brawer <brawer@dandelis.ch>

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

package gnu.testlet.java.util.logging.Logger;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Logger;
import java.util.MissingResourceException;



/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getLogger
  implements Testlet
{
  TestSecurityManager sec = new TestSecurityManager();

  public void test(TestHarness h)
  {
    Logger fooLogger = null;
    Logger barLogger = null;
    Throwable caught;
    final String loggerName = TestLogger.class.getName();

    try
      {
        sec.install();

        // Check #1.
        sec.setGrantLoggingControl(false);
        h.check(Logger.getLogger("global") == Logger.global);

        // Check #2.
        sec.setGrantLoggingControl(false);
        caught = null;
        try
          {
            fooLogger = Logger.getLogger(
              "gnu.testlet.java.util.logging.Logger.TestLogger");
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        h.check(caught instanceof SecurityException);

        // Check #3.
        sec.setGrantLoggingControl(true);
        try
          {
            fooLogger = Logger.getLogger(loggerName);
            h.check(true);
          }
        catch (Exception ex)
          {
            h.check(false);
            h.debug(ex);
          }

        // Check #4.
        sec.setGrantLoggingControl(false);
        h.check(fooLogger.getName(), loggerName);

        // Check #5.
        sec.setGrantLoggingControl(false);
        h.check(fooLogger.getResourceBundle() == null);

        // Check #6: Can retrieve existing logger without LoggingPermission.
        sec.setGrantLoggingControl(false);
        h.check(Logger.getLogger(loggerName) == fooLogger);

        // Check #7.
        sec.setGrantLoggingControl(false);
        try
          {
            Logger.getLogger(loggerName, "NonexistingResource");
            h.check(false);
          }
        catch (MissingResourceException _)
          {
            h.check(true);
          }
        catch (Exception ex)
          {
            h.check(false);
            h.debug(ex);
          }

        // Check #8.
        sec.setGrantLoggingControl(false);
        barLogger = Logger.getLogger(loggerName,
                                     TestResourceBundle.class.getName());
        h.check(barLogger == fooLogger);

        // Check #9.
        //
        // Sun J2SE 1.4.1_01 fails this test. However, it seems wrong
        // that a call to getLogger should be able to install a
        // ResourceBundle into a pre-existing logger if the caller
        // does not have LoggingPermission.
        //
        // Sun Microsystems has been informed about this security-related
        // bug in the reference implementation by submitting a bug report.
        // Sun bug review ID: 240615.
        h.check(fooLogger.getResourceBundle() == null);
      }
    finally
      {
        sec.uninstall();
      }
  }
}
