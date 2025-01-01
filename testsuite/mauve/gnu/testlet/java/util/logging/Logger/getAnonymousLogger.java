// Tags: JDK1.4
// Uses: TestFilter TestSecurityManager TestResourceBundle

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

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.Formatter;
import java.util.logging.SimpleFormatter;

import java.util.MissingResourceException;


/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class getAnonymousLogger
  implements Testlet
{
  TestSecurityManager sec = new TestSecurityManager();

  public void test(TestHarness th)
  {
    Logger al;
    Throwable caught;
    TestFilter filter = new TestFilter();
    Formatter formatter = new SimpleFormatter();

    try
      {
        sec.install();
        // This used to be 'sec.setGrantLoggingControl(false)', but that
        // causes Logger.getAnonymousLogger() to fail on JDK 1.4.2.  
        // Stephen Crawley: 2004-05-11
        sec.setGrantLoggingControl(true);

        // Check #1.
        al = Logger.getAnonymousLogger();
        th.check(al != null);

        // Check #2: New instance for each call.
        th.check(al != Logger.getAnonymousLogger());

        // Check #3.
        al = Logger.getAnonymousLogger();
        th.check(al.getName(), null);

        // Check #4.
        th.check(al.getResourceBundle(), null);

        // Check #5.
        th.check(al.getResourceBundleName(), null);

        // Check #6: Parent is root logger.
        th.check(al.getParent(), Logger.getLogger(""));

        // Check #7.
        al.setFilter(filter);
        al.setUseParentHandlers(false);
        al.setLevel(Level.FINEST);
        al.entering("Class", "method", "txt");
        th.check(formatter.formatMessage(filter.getLastRecord()), "ENTRY txt");

        // Check #8.
        al = Logger.getAnonymousLogger(TestResourceBundle.class.getName());
        th.check(al.getResourceBundle() instanceof TestResourceBundle);

        // Check #9.
        al.setFilter(filter);
        al.setUseParentHandlers(false);
        al.setLevel(Level.FINEST);
        al.entering("Class", "method", "txt");
        th.check(formatter.formatMessage(filter.getLastRecord()), "BETRETEN txt");

        // Check #10.
        try
          {
            Logger.getAnonymousLogger("garbageClassName");
            th.check(false);
          }
        catch (MissingResourceException ex)
          {
            th.check(true);
          }
        catch (Exception ex)
          {
            th.check(false);
            th.debug(ex);
          }
      }
    finally
      {
        sec.uninstall();
      }
  }
}
