/* exportToClipboard.java -- Tests TransferHandler.exportToClipboard()
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

import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;

import javax.swing.JComponent;
import javax.swing.TransferHandler;

import junit.framework.TestCase;

/**
 * Tests TransferHandler.exportToClipboard().
 */
public class exportToClipboard extends TestCase
{

  /**
   * Overrides setContents() to enable us to check what is passed to
   * setContents().
   */
  private class TestClipboard
    extends Clipboard
  {
    TestClipboard()
    {
      super("DEFAULT");
    }
    public void setContents(Transferable t, ClipboardOwner o)
    {
      super.setContents(t, o);
      clipboardOwner = o;
    }
  }

  /**
   * Overrides exportDone() to enable checking when this is called and what
   * is passed to it.
   */
  private class TestTransferHandler
    extends TransferHandler
  {
    TestTransferHandler(String prop)
    {
      super(prop);
    }
    public void exportDone(JComponent c, Transferable t, int a)
    {
      exportDoneSource = c;
      exportDoneTransferable = t;
      exportDoneAction = a;
    }

    public int getSourceActions(JComponent c)
    {
      return sourceActions;
    }
  }

  /**
   * Adds a readonly property that returns a fixed value.
   */
  public class TestComponent
    extends JComponent
  {
    public String getTestProperty()
    {
      return "HelloWorld";
    }
  }

  // The value that will be returned by getSourceActions(), for testing.
  int sourceActions;

  // The values passed into exportDone for testing.
  JComponent exportDoneSource;
  Transferable exportDoneTransferable;
  int exportDoneAction;

  /**
   * The clipboard owner beeing passed to Clipboard.stContents().
   */
  ClipboardOwner clipboardOwner;

  /**
   * The transfer handler used in these tests.
   */
  private TransferHandler transferHandler;

  /**
   * The component from which we export a property.
   */
  private JComponent component;

  /**
   * The clipboard to use.
   */
  private Clipboard clipboard;

  /**
   * Sets up the testcase.
   */
  public void setUp()
  {
    transferHandler = new TestTransferHandler("testProperty");
    component = new TestComponent();
    clipboard = new TestClipboard();
    exportDoneAction = -1;
    exportDoneSource = null;
    exportDoneTransferable = null;
    clipboardOwner = null;
  }

  /**
   * Tears down the testcase.
   */
  public void tearDown()
  {
    transferHandler = null;
    component = null;
    clipboard = null;
    exportDoneAction = -1;
    exportDoneSource = null;
    exportDoneTransferable = null;
    clipboardOwner = null;
  }

  /**
   * Checks how 'intersecting' source actions are handled.
   */
  public void testIntersectingSourceActions()
  {
    sourceActions = TransferHandler.COPY;
    transferHandler.exportToClipboard(component, clipboard,
                                      TransferHandler.MOVE);
    assertNull(clipboard.getContents(this));
    assertEquals(component, exportDoneSource);
    assertNull(exportDoneTransferable);
    assertEquals(TransferHandler.NONE, exportDoneAction);
  }


  /**
   * Tests how a missing property getter method is handled.
   */
  public void testMissingGetter()
  {
    component = new JComponent(){};
    sourceActions = TransferHandler.COPY;
    transferHandler.exportToClipboard(component, clipboard,
                                      TransferHandler.COPY);
    assertNull(clipboard.getContents(this));
    assertEquals(component, exportDoneSource);
    assertNull(exportDoneTransferable);
    assertEquals(TransferHandler.NONE, exportDoneAction);
  }

  /**
   * Tests how a normal transfer is performed.
   */
  public void testNormalTransfer()
  {
    sourceActions = TransferHandler.COPY;
    transferHandler.exportToClipboard(component, clipboard,
                                      TransferHandler.COPY);
    assertEquals(component, exportDoneSource);
    assertNotNull(exportDoneTransferable);
    assertSame(exportDoneTransferable, clipboard.getContents(this));

    try
      {
        DataFlavor flavor = new DataFlavor("application/x-java-jvm-local-objectref; class=java.lang.String");
        assertEquals("HelloWorld",
                     exportDoneTransferable.getTransferData(flavor));
      }
    catch (Exception ex)
      {
        ex.printStackTrace();
        fail(ex.getMessage());
      }
    assertEquals(TransferHandler.COPY, exportDoneAction);
    assertNull(clipboardOwner);
  }

  /**
   * Tests how an IllegalStateException from the ClipBoard is handled.
   */
  public void testIllegalStateException()
  {
    sourceActions = TransferHandler.COPY;
    clipboard = new Clipboard("DEFAULT")
    {
      public void setContents(Transferable t, ClipboardOwner o)
      {
        throw new IllegalStateException();
      }
    };
    try
      {
        transferHandler.exportToClipboard(component, clipboard,
                                          TransferHandler.COPY);
        fail("IllegalStateException must be thrown");
      }
    catch (IllegalStateException ex)
      {
        // Ok.
      }

    assertEquals(component, exportDoneSource);
    assertNotNull(exportDoneTransferable);
    assertEquals(TransferHandler.NONE, exportDoneAction);
  }

}
