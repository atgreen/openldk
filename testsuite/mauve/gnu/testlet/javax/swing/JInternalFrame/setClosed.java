/* setClosed.java -- Checks the setClosed method
   Copyright (C) 2006 Roman Kennke (kennke@aicas.com)
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.javax.swing.JInternalFrame;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;

import javax.swing.JInternalFrame;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class setClosed implements Testlet
{

  /**
   * Catches property changes and stores the property name.
   */
  class TestPropertyChangeHandler
    implements PropertyChangeListener
  {

    public void propertyChange(PropertyChangeEvent e)
    {
      propertyChanged = e.getPropertyName();
    }
    
  }

  /**
   * Stores the name of the last changed property.
   */
  String propertyChanged;

  /**
   * The entry point in this test.
   *
   * @param harness the test harness to use
   */
  public void test(TestHarness harness)
  {
    testBoundProperty(harness);
  }

  /**
   * Tests if this is a bound property.
   *
   * @param h the test harness to use
   */
  private void testBoundProperty(TestHarness h)
  {
    h.checkPoint("testBoundProperty");
    JInternalFrame t = new JInternalFrame();
    t.addPropertyChangeListener(new TestPropertyChangeHandler());
    try
      {
        t.setClosed(false);
        propertyChanged = null;
        t.setClosed(true);
        h.check(propertyChanged, "closed");
      }
    catch (PropertyVetoException ex)
      {
        h.fail(ex.getMessage());
      }
  }
}
