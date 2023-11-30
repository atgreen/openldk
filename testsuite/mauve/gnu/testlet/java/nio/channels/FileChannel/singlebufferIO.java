/* singlebufferIO.java -- FileChannel test using a single buffer
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
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;

/**
 * @author mike
 */
public class singlebufferIO
    implements Testlet
{
  /*
   * (non-Javadoc)
   * 
   * @see gnu.testlet.Testlet#test(gnu.testlet.TestHarness)
   */
  public void test(TestHarness harness)
  {
    byte[] data = "qwertyuiopasdfghjklzxcvbnm".getBytes();
    ByteBuffer out = ByteBuffer.wrap(data);
    ByteBuffer in = ByteBuffer.wrap(new byte[data.length]);
    String tmpfile = harness.getTempDirectory() + File.separator
                     + "mauve-singlebuffer.tmp";
    try
      {
        File f = new File(tmpfile);
        f.createNewFile();
        FileChannel fcOut = new FileOutputStream(f).getChannel();
        fcOut.write(out);
        fcOut.close();
        harness.check(out.position() == out.limit(), "Position - Limit mismatch");

        FileChannel fcIn = new FileInputStream(f).getChannel();
        fcIn.read(in);

        System.out.println("Position: " + in.position() + ", Limit: " + in.limit());
        System.out.println("Position: " + out.position() + ", Limit: " + out.limit());
        harness.check(Arrays.equals(out.array(), in.array()));
      }
    catch (IOException e)
      {
        harness.fail("Unexpected exception: " + e);
      }
    finally
      {
        // delete the work file and check if deletion were successfull
        if (tmpfile != null)
          {
            new File(tmpfile).delete();
          }
      }
  }

}
