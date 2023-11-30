// Tags: JDK1.4

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

package gnu.testlet.java.util.logging.LogRecord;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.ListResourceBundle;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.LogRecord;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class setResourceBundle
  implements Testlet
{
  public void test(TestHarness th)
  {
    LogRecord rec = new LogRecord(Level.CONFIG, "foo");
    ResourceBundle testBundle = new TestResourceBundle();

    // Check #1.
    th.check(rec.getResourceBundle(), null);

    // Check #2.
    rec.setResourceBundle(testBundle);
    th.check(rec.getResourceBundle() == testBundle);

    // Check #3.
    rec.setResourceBundle(null);
    th.check(rec.getResourceBundle(), null);
  }


  public static class TestResourceBundle
    extends ListResourceBundle
  {
    private final Object[][] contents = new Object[][]
      {
	{ "test1", "foo-bar-baz" },
	{ "test2", "the single parameter is {0}" },
	{ "test3", "the two parameters are {0} and {1}" }
      };

    protected Object[][] getContents()
    {
      return contents;
    }
  };
}
