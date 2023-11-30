// Tags: JDK1.4
// Uses: TestLogger TestSecurityManager

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

import java.util.logging.*;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class securityChecks
  implements Testlet
{
  private final TestSecurityManager sec = new TestSecurityManager();
  private TestHarness harness;

  public void test(TestHarness h)
  {
    this.harness = h;
    try
      {
        sec.install();

        testSecurity("Logger.global", Logger.global,
                     /* expect enforcement */ true);

        testSecurity("Logger.getAnonymousLogger()",
                     Logger.getAnonymousLogger(),
                     /* expect enforcement */ false);

        testSecurity("Logger.getAnonymousLogger(null)",
                     Logger.getAnonymousLogger(null),
                     /* expect enforcement */ false);

        testSecurity("<named custom logger>",
                     new TestLogger("foo", null),		 
                     /* expect enforcement */ true);

        testSecurity("<anonymous custom logger>",
                     new TestLogger(null, null),		 
                     /* expect enforcement */ true);
      }
    finally
      {
        sec.uninstall();
      }
  }


  private void testSecurity(String checkPoint, Logger logger,
                            boolean expectEnforcement)
  {
    harness.checkPoint(checkPoint);
    
    // Check #1: setFilter(null) [no LoggingPermission]
    sec.setGrantLoggingControl(false);
    try
      {
        logger.setFilter(null);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #2: setFilter(f) [no LoggingPermission].
    try
      {
        logger.setFilter(new Filter()
          {
            public boolean isLoggable(LogRecord r)
            {
              return true;
            }
          });
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #3: setLevel(null) [no LoggingPermission].
    try
      {
        logger.setLevel(null);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #4: setLevel(Level.CONFIG) [no LoggingPermission].
    try
      {
        logger.setLevel(Level.CONFIG);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #5: addHandler(h) [no LoggingPermission].
    Handler handler = new ConsoleHandler();
    try
      {
        logger.addHandler(handler);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #6: removeHandler(h) [no LoggingPermission].
    try
      {
        logger.removeHandler(handler);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }


    // Check #7: setParent(l) [no LoggingPermission].
    try
      {
        logger.setParent(new TestLogger("gnu.testlet.Test", null));
        harness.check(false);
      }
    catch (Exception ex)
      {
        harness.check(ex instanceof SecurityException);
      }


    // Check #8: setUseParentHandlers(false) [no LoggingPermission].
    try
      {
        logger.setUseParentHandlers(false);
        harness.check(!expectEnforcement);
      }
    catch (Exception ex)
      {
        harness.check(expectEnforcement && (ex instanceof SecurityException));
      }
  }
}
