// Tags: JDK1.4
// Uses: TestHandler TestSecurityManager

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

package gnu.testlet.java.util.logging.Handler;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.ErrorManager;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getErrorManager
  implements Testlet
{
  private final TestSecurityManager sec = new TestSecurityManager();
  private final TestHandler handler = new TestHandler();
  private final ErrorManager mgr = new ErrorManager();

  public void test(TestHarness th)
  {
    Throwable caught;

    sec.install();
    try
      {
        // Check #1: getErrorManager() [no permission]
        sec.setGrantLoggingControl(false);
        caught = null;
        try
          {
            handler.getErrorManager();
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        th.check(caught instanceof SecurityException);

        // Check #2 and #3: getErrorManager() [with permission]
        sec.setGrantLoggingControl(true);
        caught = null;
        try
          {
            handler.setErrorManager(mgr);
          }
        catch (Exception ex)
          {
            caught = ex;
          }
        th.check(caught, null);                      // Check #2.
        th.check(handler.getErrorManager() == mgr);  // Check #3.
      }
    finally
      {
        sec.uninstall();
      }
  }
}
