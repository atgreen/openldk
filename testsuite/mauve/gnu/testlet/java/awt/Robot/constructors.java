// Tags: GUI JDK1.3

// Copyright (C) 2004 Red Hat

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

package gnu.testlet.java.awt.Robot;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.awt.Robot;
import java.awt.AWTException;

/**
 * Checks that constructors in the {@link Robot} class work 
 * correctly.
 */
public class constructors implements Testlet
{

  /**
   * Runs the test using the specified harness.
   * 
   * @param harness  the test harness (<code>null</code> not permitted).
   */
  public void test (TestHarness harness)
  {
    // default constructor
    try
      {
	Robot r = new Robot ();
      }
    catch (AWTException e)
      {
	harness.fail ("caught AWT exception: " + e.getMessage ());
      }

    // the test passed if we got here
    harness.check (true);
  }
}
