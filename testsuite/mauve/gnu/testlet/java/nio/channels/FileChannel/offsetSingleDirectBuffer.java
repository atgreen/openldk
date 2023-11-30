/* offsetSingleBuffer.java -- Test writing offset from a single direct buffer
   Copyright (C) 2006 Michael Barker
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

// Tags: JDK14

package gnu.testlet.java.nio.channels.FileChannel;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;

/**
 * @author mike
 *
 */
public class offsetSingleDirectBuffer implements Testlet
{

  /* (non-Javadoc)
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness) 
  {
    try
      {
        byte[] data = "qwertyuiopasdfghjklzxcvbnm".getBytes("UTF-8");
        ByteBuffer out = ByteBuffer.allocateDirect(50);
        out.put(data);
        out.flip();
        out.position(5);
        ByteBuffer in = ByteBuffer.allocateDirect(50);
        
        String tmpfile = harness.getTempDirectory() + File.separator
        + "mauve-offset-single-direct-buffer.tmp";
        File f = new File(tmpfile);
        
        FileOutputStream fOut = new FileOutputStream(f);
        FileChannel fc = fOut.getChannel();
        int numBytes = fc.write(out);
        harness.check(numBytes, data.length - 5, "Number of bytes written");
        fc.close();
        
        harness.check(f.length(), data.length - 5, "Resulting File size");
        
        in.position(5);
        FileInputStream fIn = new FileInputStream(f);
        FileChannel fcIn = fIn.getChannel();
        int numRead = fcIn.read(in);
        harness.check(numRead, data.length - 5, "Number of bytes read");
        harness.check(in.position(), data.length, "Buffer position");
        byte[] oldData = new byte[data.length - 5];
        System.arraycopy(data, 5, oldData, 0, 21);
        byte[] newData = new byte[data.length - 5];
        in.flip();
        in.position(5);
        in.get(newData);
        harness.check(Arrays.equals(newData, oldData), "File content");
        
        fcIn.close();
        
        f.delete();
      }
    catch (UnsupportedEncodingException e1)
      {
        harness.fail("Unsupported Encoding");
      }
    catch (SecurityException e)
      {
        harness.fail("Unexpected exception: " + e);
      }
    catch (FileNotFoundException e)
      {
        harness.fail("Unexpected exception: " + e);
      }
    catch (IOException e)
      {
        harness.fail("Unexpected exception: " + e);
      }
    
    
  }

}
