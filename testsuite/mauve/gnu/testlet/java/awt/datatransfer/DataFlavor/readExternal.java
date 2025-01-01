/* readExternal.java -- Tests DataFlavor.readExternal()
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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import junit.framework.TestCase;

/**
 * Tests DataFlavor.readExternal().
 */
public class readExternal extends TestCase
{
  public void testBasicRead()
  {
    // Write out using writeExternal().
    try
      {
        DataFlavor f = new DataFlavor("application/text; param1=xyz",
        "Plain Text");
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);
        f.writeExternal(oos);
        ByteArrayInputStream bais =
          new ByteArrayInputStream(baos.toByteArray());
        ObjectInputStream ois = new ObjectInputStream(bais);
        DataFlavor read = new DataFlavor();
        read.readExternal(ois);
        assertEquals(f.getPrimaryType(), read.getPrimaryType());
        assertEquals(f.getSubType(), read.getSubType());
        assertEquals(f.getRepresentationClass(),
                     read.getRepresentationClass());
        assertEquals(f.getHumanPresentableName(),
                     read.getHumanPresentableName());
        assertEquals("xyz", read.getParameter("param1"));
        assertNull(read.getParameter("humanPresentableName"));
        assertEquals(f.getRepresentationClass().getName(),
                     read.getParameter("class"));
      }
    catch (IOException ex)
      {
        fail();
      }
    catch (ClassNotFoundException ex)
      {
        fail();
      }
  }
}
