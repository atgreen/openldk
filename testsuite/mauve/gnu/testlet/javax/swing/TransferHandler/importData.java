/* importData.java -- Tests the TransferHandler importData() method
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
import gnu.testlet.TestHarness;

/**
 * Tests the TransferHandler.importData() method.
 */
public class importData extends TestCase
{

  /**
   * Implements test properties in JComponent.
   */
  public static class TestComponent extends JComponent
  {
    String property = null;
    public void setTestProperty(String prop)
    {
      property = prop;
    }
    public String getTestProperty()
    {
      return property;
    }
  }

  /**
   * A Transferable used for our tests.
   */
  static class TestTransferable implements Transferable
  {

    public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException
    {
      return "Hello World";
    }

    public DataFlavor[] getTransferDataFlavors()
    {
      try
        {
          DataFlavor flavor = new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType
                                             + "; class=java.lang.String");
          return new DataFlavor[]{ flavor };
        }
      catch (ClassNotFoundException ex)
        {
          throw new InternalError();
        }
    }

    public boolean isDataFlavorSupported(DataFlavor flavor)
    {
      return flavor.getPrimaryType().equals("application")
             && flavor.getSubType().equals("x-java-jvm-local-objectref")
             && flavor.getRepresentationClass().equals("java.lang.String");
    }
    
  }

  /**
   * The component to which we transfer.
   */
  private JComponent component;

  /**
   * The transfer handler that we use.
   */
  private TransferHandler transferHandler;

  /**
   * The transferable that gets transferred.
   */
  private Transferable transferable;

  /**
   * Sets up the test case.
   */
  public void setUp()
  {
    component = new TestComponent();
    transferHandler = new TransferHandler("testProperty");
    transferable = new TestTransferable();
  }

  /**
   * Tears down the test case.
   */
  public void tearDown()
  {
    component = null;
    transferHandler = null;
    transferable = null;
  }

  /**
   * Tests a normal transfer.
   */
  public void testOK()
  {
    boolean ok = transferHandler.importData(component, transferable);
    assertTrue(ok);
    assertEquals(((TestComponent) component).getTestProperty(),
                 "Hello World");
  }

  /**
   * Tests a transfer with a transferable that does not support the correct
   * flavor.
   */
  public void testWrongFlavor()
  {
    transferable = new TestTransferable()
    {
      public DataFlavor[] getTransferDataFlavors()
      {
        return new DataFlavor[0];
      }
    };
    boolean ok = transferHandler.importData(component, transferable);
    assertFalse(ok);
    assertNull(((TestComponent) component).getTestProperty());
  }

  /**
   * Tests a transfer to a component that doesn't have the correct property
   * write method.
   */
  public void testInvalidWriter()
  {
    component = new JComponent()
    {
      public void setTestProperty(String prop, int i)
      {
        
      }
      public String getTestProperty()
      {
        return "test";
      }
    };
    boolean ok = transferHandler.importData(component, transferable);
    assertFalse(ok);
  }

  /**
   * Tests a transfer to a component without the property writer.
   */
  public void testMissingPropertyWriter()
  {
    component = new JComponent()
    {
      public String getTestProperty()
      {
        return "test";
      }
    };
    boolean ok = transferHandler.importData(component, transferable);
    assertFalse(ok);
  }

  /**
   * Tests a transfer to a component without the property reader.
   */
  public void testMissingPropertyReader()
  {
    component = new JComponent()
    {
      public void setTestProperty(String prop)
      {
        
      }
    };
    boolean ok = transferHandler.importData(component, transferable);
    assertFalse(ok);
  }

  /**
   * Tests a transfer with a TransferHandler initialized with a null
   * property.
   */
  public void testNullProperty()
  {
    transferHandler = new TransferHandler(null);
    boolean ok = transferHandler.importData(component, transferable);
    assertFalse(ok);
  }

}
