/* mulitdirectbufferIO.java -- Scatter/Gather IO using direct buffers
 Copyright (C) 2006 Michael Barker
 Updated for OpenJDK7 Pavel Tisnovsky
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;

/**
 * @author mike
 */
public class multidirectbufferIO
    implements Testlet
{

  private void initBuffer(ByteBuffer[] bs, byte[] data)
  {
    for (int i = 0; i < bs.length; i++)
      {
        bs[i] = ByteBuffer.allocateDirect(data.length);
        bs[i].put(data);
        bs[i].flip();
      }
  }

  /*
   * (non-Javadoc)
   * 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    final int BUF_LEN = 17;
    final int MAX_BUFFERS = 16;
    byte[] data = "qwertyuiopasdfghjklzxcvbnm".getBytes();
    ByteBuffer[] out = new ByteBuffer[BUF_LEN];
    ByteBuffer[] in = new ByteBuffer[BUF_LEN];
    initBuffer(out, data);
    initBuffer(in, new byte[data.length]);
    String tmpfile = harness.getTempDirectory() + File.separator
                     + "mauve-multibuffer.tmp";
    try
      {
        File f = new File(tmpfile);
        f.createNewFile();
        FileChannel fcOut = new FileOutputStream(f).getChannel();
        long numWritten = fcOut.write(out);
        fcOut.close();
        /* The SUN JDK limits the number of buffers to 16 */
        /* This has been fixed in OpenJDK 7u8 */
        harness.check(numWritten, conformToJDK17_u8() ? (BUF_LEN * data.length) : (MAX_BUFFERS * data.length));
        for (int i = 0; i < MAX_BUFFERS; i++)
          {
            harness.check(out[i].position() == out[i].limit(), "Position - Limit mismatch");
          }

        FileChannel fcIn = new FileInputStream(f).getChannel();
        long numRead = fcIn.read(in);
        /* The SUN JDK limits the number of buffers to 16 */
        /* This has been fixed in OpenJDK 7u8 */
        harness.check(numRead, conformToJDK17_u8() ? (BUF_LEN * data.length) : (MAX_BUFFERS * data.length));

        for (int i = 0; i < MAX_BUFFERS; i++)
          {
            byte[] dIn = new byte[data.length];
            byte[] dOut  = new byte[data.length];
            in[i].flip();
            out[i].flip();
            in[i].get(dIn);
            out[i].get(dOut);
            harness.check(Arrays.equals(dIn, dOut));
          }
        
        f.delete();
      }
    catch (IOException e)
      {
        harness.fail("Unexpected exception: " + e);
      }
  }

  /**
    * Returns true if tested JRE conformns to JDK 1.7 update 8 or higher.
    * @author: Pavel Tisnovsky
    */
  private static boolean conformToJDK17_u8()
  {
    String[] javaVersion = System.getProperty("java.version").split("\\.");
    String vendorID = System.getProperty("java.vendor");
    // test of OpenJDK
    if ("Sun Microsystems Inc.".equals(vendorID) || "Oracle Corporation".equals(vendorID) )
    {
      int version = Integer.parseInt(javaVersion[1]);
      if (version > 7) {
        return true;
      }
      if (version == 7) {
        String[] splitstr = javaVersion[2].split("_");
        int update = Integer.parseInt(splitstr[1]);
        return update >= 8;
      }
    }
    return false;
  }

}
