/* createTransferable.java -- Tests TransferHandler.createTransferable()
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
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

import junit.framework.TestCase;

/**
 * Tests the TransferHandler.createTransferable() method.
 */
public class createTransferable extends TestCase
{

  /**
   * Overridden to make createTransferable() public for testing.
   */
  private class TestTransferHandler extends TransferHandler
  {
    TestTransferHandler(String prop)
    {
      super(prop);
    }
    public Transferable createTransferable(JComponent comp)
    {
      return super.createTransferable(comp);
    }
  }

  public class TestComponent
    extends JComponent
  {
    private String value;
    public void setTestProperty(String val)
    {
      value = val;
    }
    public String getTestProperty()
    {
      return "HelloWorld";
    }
  }

  /**
   * The transfer handler that we test.
   */
  private TestTransferHandler transferHandler;

  /**
   * The component that is passed in the createTransferable() method.
   */
  private JComponent component;

  public void setUp()
  {
    component = new TestComponent();
    transferHandler = new TestTransferHandler("testProperty");
  }

  public void tearDown()
  {
    component = null;
    transferHandler = null;
  }

  /**
   * Check what is returned when we initialize the TransferHandler with a
   * null property.
   */
  public void testNullProperty()
  {
    transferHandler = new TestTransferHandler(null);
    Transferable transferable = transferHandler.createTransferable(component);
    assertNull(transferable);
  }

  public void testMissingGetter()
  {
    component = new JComponent()
    {
      String value;
      public void setTestProperty(String val)
      {
        value = val;
      }
    };
    Transferable transferable = transferHandler.createTransferable(component);
    assertNull(transferable);
  }

  public void testMissingSetter()
  {
    component = new JComponent()
    {
      String value;
      public String getTestProperty()
      {
        return value;
      }
    };
    Transferable transferable = transferHandler.createTransferable(component);
    assertNotNull(transferable);
  }

  public void testAllOk()
  {
    Transferable transferable = transferHandler.createTransferable(component);
    assertNotNull(transferable);
  }

  /**
   * Tests which transfer flavors are supported by the created Transferable.
   */
  public void testTransferableTransferFlavors()
  {
    Transferable transferable = transferHandler.createTransferable(component);
    DataFlavor[] flavors = transferable.getTransferDataFlavors();
    assertEquals(1, flavors.length);
    assertEquals("application/x-java-jvm-local-objectref; class=java.lang.String", flavors[0].getMimeType());
  }

  public void testTransferableDataFlavorSupported()
  {
    Transferable transferable = transferHandler.createTransferable(component);
    try
      {
        // Primary type doesn't match.
        DataFlavor flavor = new DataFlavor("xyz/x-java-jvm-local-objectref; class=java.lang.String");
        assertFalse(transferable.isDataFlavorSupported(flavor));
        // Subtype type doesn't match.
        flavor = new DataFlavor("application/x-java-remote-object; class=java.lang.String");
        assertFalse(transferable.isDataFlavorSupported(flavor));
        // Representation class doesn't match.
        flavor = new DataFlavor("application/x-java-jvm-local-objectref; class=java.lang.Integer");
        assertFalse(transferable.isDataFlavorSupported(flavor));
                    
        // Good match.
        flavor = new DataFlavor("application/x-java-jvm-local-objectref; class=java.lang.String");
        assertTrue(transferable.isDataFlavorSupported(flavor));
      }
    catch (ClassNotFoundException ex)
      {
        fail(ex.getMessage());
      }
  }

  public void testTransferableTransferData()
  {
    Transferable transferable = transferHandler.createTransferable(component);
    // Try invalid data flavor.
    try
      {
        DataFlavor flavor = new DataFlavor("xyz/x-java-jvm-local-objectref; class=java.lang.String");
        Object data = transferable.getTransferData(flavor);
        fail("UnsupportedOperationException must be thrown");
      }
    catch (UnsupportedFlavorException ex)
      {
        // Ok.
      }
    catch (IOException ex)
      {
        fail("UnsupportedFlavorException must be thrown, no IOException");
      }
    catch (ClassNotFoundException ex)
      {
        fail(ex.getMessage());
      }

    // Try OK data flavor.
    try
      {
        DataFlavor flavor = new DataFlavor("application/x-java-jvm-local-objectref; class=java.lang.String");
        Object data = transferable.getTransferData(flavor);
        assertEquals(data, "HelloWorld");
      }
    catch (UnsupportedFlavorException ex)
      {
        fail("UnsupportedFlavorException must not be thrown");
      }
    catch (IOException ex)
      {
        System.err.println(ex.getMessage());
        fail("IOException must not be thrown");
      }
    catch (ClassNotFoundException ex)
      {
        fail(ex.getMessage());
      }
    // Try non-accessible component.
    component = new JComponent()
      {
        public String getTestProperty()
        {
          return "Hello World";
        }
      };
    transferable = transferHandler.createTransferable(component);

    try
      {
        DataFlavor flavor = new DataFlavor("application/x-java-jvm-local-objectref; class=java.lang.String");
        Object data = transferable.getTransferData(flavor);
        fail("IOException must be thrown");
      }
    catch (UnsupportedFlavorException ex)
      {
        fail("UnsupportedFlavorException must not be thrown");
      }
    catch (IOException ex)
      {
        // Ok.
      }
    catch (ClassNotFoundException ex)
      {
        fail(ex.getMessage());
      }
  }
}
