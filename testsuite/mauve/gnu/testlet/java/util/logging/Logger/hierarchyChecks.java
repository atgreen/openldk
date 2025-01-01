// Tags: JDK1.4
// Uses: TestFilter TestLogger TestResourceBundle TestSecurityManager

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
 * Performs various checks on the hierarchy of loggers.
 *
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class hierarchyChecks
  implements Testlet
{
  private final TestSecurityManager sec = new TestSecurityManager();

  private static final String PARENT_NAME
    = "gnu.testlet.java.util.logging.Logger.hierarchyChecks";

  private static final String CHILD_NAME
    = PARENT_NAME + ".child";

  public void test(TestHarness h)
  {
    Logger parent, child;
    TestFilter parentFilter, childFilter;
    LogRecord rec;
    Formatter formatter;

    try
      {
        // Preparation.
        sec.install();
        parentFilter = new TestFilter();
        childFilter = new TestFilter();
        formatter = new SimpleFormatter();

        sec.setGrantLoggingControl(true);
        parent = Logger.getLogger(PARENT_NAME,
                                  TestResourceBundle.class.getName());
        parent.setLevel(Level.ALL);
        parent.setFilter(parentFilter);
        parent.setUseParentHandlers(false);
        child = Logger.getLogger(CHILD_NAME);
        child.setFilter(childFilter);
        child.setLevel(Level.ALL);
        sec.setGrantLoggingControl(false);

        // Check #1.
        h.check(parent.getResourceBundle() instanceof TestResourceBundle);

        // Check #2.
        h.check(child.getResourceBundle() == null);

        // Check #3.
        h.check(parent.getResourceBundleName(),
                TestResourceBundle.class.getName());

        // Check #4.
        h.check(child.getResourceBundleName(), null);

        // Check #5.
        h.check(child.getParent() == parent);

        // Check #6.
        h.check(parent.getUseParentHandlers() == false);

        // Check #7.
        h.check(child.getUseParentHandlers() == true);
        
        // Check #8: child uses parent's ResourceBundle.
        child.warning("Boo!");
        h.check(childFilter.getLastRecord().getResourceBundle()
                == parent.getResourceBundle());

        // Check #9.
        h.check(parentFilter.getLastRecord() == null);

        // Check #10: log() passes ResourceBundle to LogRecord without
        // localization.
        child.entering("fakedClass", "fakedMethod", "SingleParam");
        rec = childFilter.getLastRecord();
        h.check(rec.getMessage(), "ENTRY {0}");

        // Check #11: Localization is then performed by Formatter.
        h.check(formatter.formatMessage(rec), "BETRETEN SingleParam");
      }
    finally
      {
        sec.uninstall();
      }
  }
}
