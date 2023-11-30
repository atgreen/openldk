// Tags: JDK1.4
// Uses: TestErrorManager TestHandler TestSecurityManager

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
public class reportError
  implements Testlet
{
  private final TestSecurityManager sec = new TestSecurityManager();
  private final TestHandler handler = new TestHandler();
  private final TestErrorManager mgr = new TestErrorManager();
  private final Exception somex = new IllegalStateException();

  public void test(TestHarness th)
  {
    sec.install();
    try
      {
        sec.setGrantLoggingControl(true);
        handler.setErrorManager(mgr);
        sec.setGrantLoggingControl(false);

        handler.invokeReportError("foo", somex,
                                  ErrorManager.FLUSH_FAILURE);

        th.check(mgr.getLastMessage(), "foo");     // Check #1.
        th.check(mgr.getLastException() == somex); // Check #2.
        th.check(mgr.getLastErrorCode(),           // Check #3.
                 ErrorManager.FLUSH_FAILURE);

        handler.invokeReportError(null, somex,
                                  ErrorManager.OPEN_FAILURE);
        th.check(mgr.getLastMessage(), null);      // Check #4.
        th.check(mgr.getLastException() == somex); // Check #5.
        th.check(mgr.getLastErrorCode(),           // Check #6.
                 ErrorManager.OPEN_FAILURE);

        handler.invokeReportError(null, null,
                                  ErrorManager.CLOSE_FAILURE);
        th.check(mgr.getLastMessage(), null);      // Check #7.
        th.check(mgr.getLastException(), null);    // Check #8.
        th.check(mgr.getLastErrorCode(),           // Check #9.
                 ErrorManager.CLOSE_FAILURE);

        handler.invokeReportError("foobar", null, -12345);
        th.check(mgr.getLastMessage(), "foobar");  // Check #10.
        th.check(mgr.getLastException(), null);    // Check #11.
        th.check(mgr.getLastErrorCode(), -12345);  // Check #12.
      }
    finally
      {
        sec.uninstall();
      }
  }
}
