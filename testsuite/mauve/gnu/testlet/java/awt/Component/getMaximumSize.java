// Tags: JDK1.4 JDK1.5

// Copyright (C) 2005 Roman Kennke  <kennke@aicas.com>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.

package gnu.testlet.java.awt.Component;

import java.awt.Component;
import java.awt.Dimension;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Checks for the default value of Component.getMaximumSize. Should be
 * (Short.MAX_VALUE, Short.MAX_VALUE).
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
public class getMaximumSize implements Testlet
{

  /**
   * Starts the test run.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    Component c = new Component(){};
    Dimension max = c.getMaximumSize();
    harness.check(max.width, Short.MAX_VALUE);
    harness.check(max.height, Short.MAX_VALUE);
  }

}
