// Tags: JDK1.4

// Copyright (C) 2004 Michael Koch <konqueror@gmx.de>

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
// Boston, MA 02111-1307, USA.


package gnu.testlet.java.nio.channels.Channels;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;
import java.nio.*;
import java.nio.channels.*;

public class ChannelsTest implements Testlet
{
  class ByteArrayChannel
    implements ReadableByteChannel, WritableByteChannel
  {
    public ByteArrayChannel()
    {
    }

    public boolean isOpen()
    {
      return true;
    }

    public void close() throws IOException
    {
    }
    
    public int read(ByteBuffer dst) throws IOException
    {
      return 0;
    }

    public int write(ByteBuffer src) throws IOException
    {
      return 0;
    }
  }
  
  public void test(TestHarness h)
  {
    String tmpfile = h.getTempDirectory() + File.separator + "mauve-channels.tst";

    File f = new File(tmpfile);

    try
      {
	RandomAccessFile raf = new RandomAccessFile(f, "rw");
	FileChannel fch = raf.getChannel();

	ByteArrayChannel bac = new ByteArrayChannel();
    
	h.checkPoint("newInputStream");
    
	InputStream in;
    
	in = Channels.newInputStream(bac);
	h.check(in != null);
    
	in = Channels.newInputStream(fch);
	h.check(in != null);

	h.checkPoint("newOutputStream");

	OutputStream out;

	out = Channels.newOutputStream(bac);
	h.check(out != null);

	out = Channels.newOutputStream(fch);
	h.check(out != null);
      }
    catch (FileNotFoundException e)
      {
	h.debug(e);
	h.fail("cannot create temporary file");
      }
    finally
      {
	f.delete();
      }
  }
}
