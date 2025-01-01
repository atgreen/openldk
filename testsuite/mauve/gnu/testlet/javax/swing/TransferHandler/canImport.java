/* canImport.java -- Checks the TransferHandler.canImport() method
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

// Tags: JDK1.4

package gnu.testlet.javax.swing.TransferHandler;

import java.awt.datatransfer.DataFlavor;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

import junit.framework.TestCase;

/**
 * Checks the TransferHandler.canImport() method.
 */
public class canImport extends TestCase
{

  /**
   * The component for which we transfer data.
   */
  private JComponent component;

  private DataFlavor[] flavors;

  /**
   * The TransferHandler.
   */
  private TransferHandler transferHandler;

  /**
   * Sets up the test environment.
   */
  public void setUp()
  {
    component = new JComponent()
    {
      String value;
      public void setTestProperty(String val)
      {
        value = val;
      }
      public String getTestProperty()
      {
        return value;
      }
    };
    transferHandler = new TransferHandler("testProperty");
    try 
      {
        DataFlavor flavor =
          new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
                         + ";class=java.lang.String");
        flavors = new DataFlavor[]{ flavor };
      }
    catch (ClassNotFoundException x)
      {
        fail(x.getMessage());
      }
  }

  /**
   * Clears the test environment.
   */
  public void tearDown()
  {
    component = null;
    transferHandler = null;
    flavors = null;
  }

  /**
   * Test a component that doesn't have a property reader method.
   */
  public void testComponentWithoutReader()
  {
    component = new JComponent()
    {
      String value;
      public void setTestProperty(String val)
      {
        value = val;
      }
    };
    boolean result = transferHandler.canImport(component, flavors);
    assertFalse(result);
  }

  /**
   * Test a component that doesn't have a property writer method.
   */
  public void testComponentWithoutWriter()
  {
    component = new JComponent()
    {
      String value;
      public String getTestProperty()
      {
        return value;
      }
    };
    boolean result = transferHandler.canImport(component, flavors);
    assertFalse(result);
  }

  /**
   * Test a wrong mime type in the data flavor.
   */
  public void testWrongMimeType()
  {
    DataFlavor flavor = DataFlavor.stringFlavor;
    flavors = new DataFlavor[]{ flavor };
    boolean result = transferHandler.canImport(component, flavors);
    assertFalse(result);
  }

  /**
   * Test a wrong representation class in the data flavor.
   */
  public void testWrongRepresentationClass()
  {
    try
      {
        DataFlavor flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
                                           + ";class=java.awt.Component");
        flavors = new DataFlavor[]{ flavor };
      }
    catch (ClassNotFoundException ex)
      {
        fail(ex.getMessage());
      }
    boolean result = transferHandler.canImport(component, flavors);
    assertFalse(result);
  }

  /**
   * Tests the return value when all parameters are ok (the component has
   * the right properties, the dataflavor is of the correct mime-type and
   * representation class).
   */
  public void testAllOK()
  {
    boolean result = transferHandler.canImport(component, flavors);
    assertTrue(result);
  }
}
