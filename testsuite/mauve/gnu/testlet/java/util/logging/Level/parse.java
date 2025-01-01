// Tags: JDK1.4
// Uses: TestUtils

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

package gnu.testlet.java.util.logging.Level;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.util.logging.Level;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
public class parse
  implements Testlet
{
  public void test(TestHarness h)
  {
    String name;
    Level level;
    int value;

    /* Pass a name. */
    for (int i = 0; i < TestUtils.LEVELS.length; i++)
      {
        name = TestUtils.NAMES[i];

        h.checkPoint(name);
        h.check(Level.parse(name) == TestUtils.LEVELS[i]);

	// Simulate a new "parsed" string as name.
        h.check(Level.parse((" " + name + " ").trim()) == TestUtils.LEVELS[i]);
      }

    /* Pass a number. */
    for (int i = 0; i < TestUtils.LEVELS.length; i++)
      {
        value = TestUtils.VALUES[i];
        name = String.valueOf(value);
        h.checkPoint(name);
        h.check(Level.parse(name), TestUtils.LEVELS[i]);
      }

    /* Parse a non-standard name. */
    h.checkPoint("non-standard name");
    try {
      Level.parse("non-standard name");
      h.check(false);
    } catch (IllegalArgumentException _) {
      h.check(true);
    } catch (Exception _) {
      h.check(false);
    }

    /* Parse a null string. */
    h.checkPoint("parse(null)");
    try {
      Level.parse(null);
      h.check(false);
    } catch (NullPointerException _) {
      h.check(true);
    } catch (Exception _) {
      h.check(false);
    }
  }
}
