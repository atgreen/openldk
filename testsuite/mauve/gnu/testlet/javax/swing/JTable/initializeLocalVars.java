// Tags: JDK1.2

// Copyright (C) 2005 Roman Kennke <kennke@aicas.com>

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.javax.swing.JTable;

import javax.swing.JTable;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * Tests if the initializeLocalVars method works correctly and if the
 * pre- and post-conditions are fullfilled.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class initializeLocalVars implements Testlet
{

  /**
   * A subclass of JTable that overrides initializeLocalVars. This is used
   * to check the preconditions of this method.
   */
  class TestTable extends JTable
  {
    protected void initializeLocalVars()
    {
      hasModel = (getModel() != null);
      hasColumnModel = (getColumnModel() != null);
      super.initializeLocalVars();
    }
  }

  boolean hasModel;
  boolean hasColumnModel;

  /**
   * Starts the test.
   *
   * @param harness the test harness to use.
   */
  public void test(TestHarness harness)
  {
    testPreconditions(harness);
  }

  /**
   * Checks the preconditions of the initializeLocalVars method.
   *
   * @param harness the test harness to use
   */
  void testPreconditions(TestHarness harness)
  {
    TestTable table = new TestTable();
    harness.check(hasModel, true);
    harness.check(hasColumnModel, true);

    // TODO: I only implement what I need atm. Complete this test.
  }
}
