// Tags: JDK1.4

// Copyright (C) 2005 Jeroen Frijters

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.nio.channels.FileChannel;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class map implements Testlet
{
  private static final byte[] msg = "Hello, World!".getBytes();

  public void test(TestHarness harness)
  {
    String filename = null;
    try
    {
        filename = harness.getTempDirectory() + File.separator + "mauvemap.txt";
        FileOutputStream fos = new FileOutputStream(filename);
        FileChannel chan = fos.getChannel();
        try
        {
            chan.map(FileChannel.MapMode.READ_WRITE, 0, msg.length);
            harness.check(false);
        }
        catch(NonReadableChannelException x)
        {
            harness.check(true);
        }
        fos.close();

        RandomAccessFile ras = new RandomAccessFile(filename, "rw");
        chan = ras.getChannel();
        MappedByteBuffer mbb = chan.map(FileChannel.MapMode.READ_WRITE, 0, msg.length);
        mbb.put(msg);
        mbb.force();

        verifyContent(harness, filename);

        MappedByteBuffer mbb2 = chan.map(FileChannel.MapMode.PRIVATE, 0, msg.length);
        mbb2.put(new byte[msg.length]);
        boolean ok = true;
        for (int i = 0; i < msg.length; i++)
            ok &= mbb2.get(i) == 0;
        harness.check(ok);
        mbb.force();
        ras.close();

        verifyContent(harness, filename);
    }
    catch(Exception x)
    {
        harness.debug(x);
        harness.check(false);
    }
    finally
    {
        // delete the work file and check if deletion were successfull
        if (filename != null)
        {
            harness.check(new File(filename).delete());
        }
    }
  }

  private void verifyContent(TestHarness harness, String filename) throws IOException
  {
      FileInputStream fis = new FileInputStream(filename);
      FileChannel chan = fis.getChannel();
      MappedByteBuffer mbb = chan.map(FileChannel.MapMode.READ_ONLY, 0, msg.length);
      byte[] buf = new byte[msg.length];
      mbb.get(buf);
      boolean ok = true;
      for (int i = 0; i < msg.length; i++)
          ok &= msg[i] == buf[i];
      harness.check(ok);
      fis.close();
  }
}
