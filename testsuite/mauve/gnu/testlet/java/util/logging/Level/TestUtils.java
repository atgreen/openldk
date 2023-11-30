// Tags: not-a-test

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

import java.util.logging.Level;

/**
 * @author <a href="mailto:brawer@dandelis.ch">Sascha Brawer</a>
 */
class TestUtils
{
  /**
   * All levels pre-defined by the specification of the Java Logging API,
   * in decreasing order of severity.
   */
  public static final Level[] LEVELS =
  {
    Level.OFF, Level.SEVERE, Level.WARNING, Level.INFO, Level.CONFIG,
    Level.FINE, Level.FINER, Level.FINEST, Level.ALL
  };


  /**
   * The expected result of calling getName() on each entry
   * in {@link #LEVELS}.
   */
  public static final String[] NAMES =
  {
    "OFF", "SEVERE", "WARNING", "INFO", "CONFIG", "FINE", "FINER",
    "FINEST", "ALL"
  };


  /**
   * The expected result of calling intValue() on each entry
   * in {@link #LEVELS}.
   */
  public static final int[] VALUES =
  {
    Integer.MAX_VALUE, 1000, 900, 800, 700, 500, 400, 300,
    Integer.MIN_VALUE
  };


  public static class CustomLevel
    extends Level
  {
    public CustomLevel(String name, int value)
    {
      super(name, value);
    }

    public CustomLevel(String name, int value, String bundleName)
    {
      super(name, value, bundleName);
    }
  };
}
