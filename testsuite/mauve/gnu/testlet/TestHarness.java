// Copyright (c) 1998, 1999, 2001  Cygnus Solutions
// Written by Tom Tromey <tromey@cygnus.com>
// Copyright (c) 2005  Mark J. Wielaard  <mark@klomp.org>

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

package gnu.testlet;

import java.awt.AWTException;
import java.awt.Robot;

import java.io.File;
import java.io.Reader;
import java.io.InputStream;

/**
 * This base class defines the API that test cases can report against.  This
 * code has been lifted from the Mauve project (and reformatted and 
 * commented).
 */
public abstract class TestHarness
  implements config
{
  /**
   * Records the result of a boolean check.
   *
   * @param result  the result.
   */
  public abstract void check (boolean result);

  /**
   * Checks the two objects for equality and records the result of
   * the check.
   *
   * @param result  the actual result.
   * @param expected  the expected result.
   */
  public void check(Object result, Object expected)
  {
    boolean ok = (result == null ? expected == null : result.equals(expected));
    check(ok);
    // This debug message may be misleading, depending on whether
    // string conversion produces same results for unequal objects.
    if (! ok)
      debug("got " + result + " but expected " + expected);
  }

  /**
   * Checks two booleans for equality and records the result of the check.
   * 
   * @param result the actual result.
   * @param expected the expected result.
   */
  public void check(boolean result, boolean expected)
  {
    boolean ok = (result == expected);
    check(ok);
    if (! ok)
      debug("got " + result + " but expected " + expected);
  }

  /**
   * Checks two ints for equality and records the result of the check.
   * 
   * @param result the actual result.
   * @param expected the expected result.
   */
  public void check(int result, int expected)
  {
    boolean ok = (result == expected);
    check(ok);
    if (! ok)
      debug("got " + result + " but expected " + expected);
  }

  /**
   * Checks two longs for equality and records the result of the check.
   * 
   * @param result the actual result.
   * @param expected the expected result.
   */
  public void check(long result, long expected)
  {
    boolean ok = (result == expected);
    check(ok);
    if (! ok)
      debug("got " + result + " but expected " + expected);
  }

  /**
   * Checks two doubles for equality and records the result of the check.
   * 
   * @param result the actual result.
   * @param expected the expected result.
   */
  public void check(double result, double expected)
  {
    // This triple check overcomes the fact that == does not
    // compare NaNs, and cannot tell between 0.0 and -0.0;
    // and all without relying on java.lang.Double (which may
    // itself be buggy - else why would we be testing it? ;)
    // For 0, we switch to infinities, and for NaN, we rely
    // on the identity in JLS 15.21.1 that NaN != NaN is true.
    boolean ok = (result == expected ? (result != 0)
                                       || (1 / result == 1 / expected)
                                    : (result != result)
                                      && (expected != expected));
    check(ok);
    if (! ok)
      // If Double.toString() is buggy, this debug statement may
      // accidentally show the same string for two different doubles!
      debug("got " + result + " but expected " + expected);
  }

  /**
   * Checks if <code>result</code> is equal to <code>expected</code> and
   * take a rounding delta into account.
   * 
   * @param result the actual result
   * @param expected the expected value
   * @param delta the rounding delta
   */
  public void check(double result, double expected, double delta)
  {
    boolean ok = true;
    if (Double.isInfinite(expected))
      {
        if (result != expected)
          ok = false;
      }
    else if (! (Math.abs(expected - result) <= delta))
      ok = false;

    check(ok);
    if (! ok)
      // If Double.toString() is buggy, this debug statement may
      // accidentally show the same string for two different doubles!
      debug("got " + result + " but expected " + expected);
  }

  // These methods are like the above, but checkpoint first.
  public void check(boolean result, String name)
  {
    checkPoint(name);
    check(result);
  }

  public void check(Object result, Object expected, String name)
  {
    checkPoint(name);
    check(result, expected);
  }

  public void check(boolean result, boolean expected, String name)
  {
    checkPoint(name);
    check(result, expected);
  }

  public void check(int result, int expected, String name)
  {
    checkPoint(name);
    check(result, expected);
  }

  public void check(long result, long expected, String name)
  {
    checkPoint(name);
    check(result, expected);
  }

  public void check(double result, double expected, String name)
  {
    checkPoint(name);
    check(result, expected);
  }

  public Robot createRobot()
  {
    Robot r = null;

    try
      {
        r = new Robot();
      }
    catch (AWTException e)
      {
        fail("TestHarness: couldn't create Robot: " + e.getMessage());
      }

    return r;
  }

  /**
   * A convenience method that sets a checkpoint with the specified name
   * then records a failed check.
   *
   * @param name  the checkpoint name.
   */
  public void fail(String name)
  {
    checkPoint(name);
    check(false);
  }
  
  // Given a resource name, return a Reader on it. Resource names are
  // just like file names.  They are relative to the top level Mauve
  // directory, but '#' characters are used in place of directory
  // separators.
  public abstract Reader getResourceReader (String name) 
    throws ResourceNotFoundException;

  public abstract InputStream getResourceStream (String name) 
    throws ResourceNotFoundException;

  public abstract File getResourceFile (String name) 
    throws ResourceNotFoundException;

  /**
   * Records a check point.  This can be used to mark a known place in a 
   * testlet.  It is useful if you have a large number of tests -- it makes 
   * it easier to find a failing test in the source code. 
   *
   * @param name  the check point name.
   */
  public abstract void checkPoint (String name);

  /**
   * This will print a message when in verbose mode.
   *
   * @param message  the message.
   */
  public abstract void verbose (String message);

  /**
   * Writes a message to the debug log.
   *
   * @param message  the message.
   */
  public abstract void debug (String message);

  /**
   * Writes a message to the debug log with or without a newline.
   *
   * @param message  the message.
   * @param newline  a flag to control whether or not a newline is added.
   */
  public abstract void debug (String message, boolean newline);
  /**
   * Writes a stack trace for the specified exception to a log.
   *
   * @param ex  the exception.
   */
  public abstract void debug (Throwable ex);

  /**
   * Writes the contents of an array to the log.
   *
   * @param o  the array of objects.
   * @param desc  the description.
   */
  public abstract void debug (Object[] o, String desc);

  // Default config interface methods.
  public String getSourceDirectory ()
  {
    return srcdir;
  }

  public String getBuildDirectory ()
  {
    return builddir;
  }
  
  /**
   * Provide a directory name for writing temporary files.
   *
   * @return The temporary directory name.
   */

  public String getTempDirectory ()
  {
    return tmpdir;
  }
  
  public String getPathSeparator ()
  {
    return pathSeparator;
  }
  
  public String getSeparator ()
  {
    return separator;
  }
  
  public String getMailHost ()
  {
    return mailHost;
  }
  public String getAutoCompile()
  {
    return autoCompile;
  }

  public String getCpInstallDir()
  {
    return cpInstallDir;
  }

  public String getEcjJar()
  {
    return ecjJar;
  }
  
  public String getEmmaString()
  {
    return emmaString;
  }

  public String getTestJava()
  {
    return testJava;
  } 
}
