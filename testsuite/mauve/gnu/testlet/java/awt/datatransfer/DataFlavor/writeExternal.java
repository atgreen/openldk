/* writeExternal.java -- Tests DataFlavor.writeExternal()
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

package gnu.testlet.java.awt.datatransfer.DataFlavor;

import java.awt.datatransfer.DataFlavor;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectOutput;
import java.util.ArrayList;

import junit.framework.TestCase;

/**
 * Tests the DataFlavor.writeExternal() method.
 */
public class writeExternal extends TestCase
{

  /**
   * An ObjectOutput implementation suitable for testing.
   */
  private class TestObjectOutput
    implements ObjectOutput
  {

    public void close() throws IOException
    {
      fail();
    }

    public void flush() throws IOException
    {
      fail();
    }

    public void write(int b) throws IOException
    {
      fail();
    }

    public void write(byte[] buf) throws IOException
    {
      fail();
    }

    public void write(byte[] buf, int offset, int len) throws IOException
    {
      fail();
    }

    public void writeObject(Object obj) throws IOException
    {
      writtenObjects.add(obj);
    }

    public void writeBoolean(boolean value) throws IOException
    {
      fail();
    }

    public void writeByte(int value) throws IOException
    {
      fail();
    }

    public void writeBytes(String value) throws IOException
    {
      fail();
    }

    public void writeChar(int value) throws IOException
    {
      fail();
    }

    public void writeChars(String value) throws IOException
    {
      fail();
    }

    public void writeDouble(double value) throws IOException
    {
      fail();
    }

    public void writeFloat(float value) throws IOException
    {
      fail();
    }

    public void writeInt(int value) throws IOException
    {
      fail();
    }

    public void writeLong(long value) throws IOException
    {
      fail();
    }

    public void writeShort(int value) throws IOException
    {
      fail();
    }

    public void writeUTF(String value) throws IOException
    {
      writtenStrings.add(value);
    }
      
  }

  /**
   * The ObjectOutput for testing.
   */
  private TestObjectOutput output;

  /**
   * Written objects, if any.
   */
  private ArrayList writtenObjects;

  /**
   * Written strings, if any.
   */
  private ArrayList writtenStrings;

  /**
   * Sets up the test case.
   */
  public void setUp()
  {
    writtenObjects = new ArrayList();
    writtenStrings = new ArrayList();
    output = new TestObjectOutput();
  }

  /**
   * Tears down the testcase.
   */
  public void tearDown()
  {
    writtenObjects = null;
    writtenStrings = null;
    output = null;
  }

  /**
   * Tests a basic serialization.
   */
  public void testWriteBasic()
  {
    DataFlavor f = new DataFlavor("application/text; param1=xyz",
                                  "Plain Text");
    try
      {
        f.writeExternal(output);
      }
    catch (IOException ex)
      {
        fail();
      }
    // Two objects are effectivly written to the stream.
    assertEquals(2, writtenObjects.size());
    assertEquals(0, writtenStrings.size());
    // The RI writes a non-public class to the ObjectOutput.
    assertEquals("java.awt.datatransfer.MimeType",
                 writtenObjects.get(0).getClass().getName());
    // And the representation class.
    assertEquals("java.lang.Class", writtenObjects.get(1).getClass().getName());
    assertSame(f.getRepresentationClass(), writtenObjects.get(1));

    // Now check how the MimeType gets serialized.
    Object o = writtenObjects.get(0);
    writtenObjects.clear();
    assertTrue(o instanceof Externalizable);
    try
      {
        ((Externalizable) o).writeExternal(output);
      }
    catch (IOException ex)
      {
        fail();
      }
    assertEquals(0, writtenObjects.size());
    assertEquals(1, writtenStrings.size());
    assertEquals("application/text; class=java.io.InputStream; param1=xyz",
                 writtenStrings.get(0));
  }
}
