// Tags: JDK1.4

// Copyright (C) 2005 Free Software Foundation, Inc.
// Written by Guilhem Lavaux (guilhem@kaffe.org)

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
package gnu.testlet.java.nio.channels.SocketChannel;

import java.net.*;
import java.util.Set;
import java.nio.*;
import java.nio.channels.*;
import java.io.OutputStream;
import java.io.InputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class select implements Testlet
{
  static final int testPort = 3487;

  public void test (TestHarness harness)
  {
    final Thread parentThread = Thread.currentThread();

    Thread t = new Thread() {
	public void run()
	{
	  try
	    {
	      Thread.sleep(10000);
	      parentThread.interrupt();
	    }
	  catch (InterruptedException e)
	    {
	    }
	}
      };
    
    t.start();

    try
      {
	ServerSocketChannel ssc = ServerSocketChannel.open();
	Selector sel = Selector.open();

	ssc.socket().bind(new InetSocketAddress(testPort));
	
	SelectionKey ssc_key;
       
	ssc.configureBlocking(true);
	try
	  {
	    ssc_key = ssc.register(sel, SelectionKey.OP_ACCEPT, null);
	    harness.fail("Channel must be in blocking mode for being able to register");
	  }
       	catch (IllegalBlockingModeException e)
	  {
	    harness.check(true);
	  }
	ssc.configureBlocking(false);
	ssc_key = ssc.register(sel, SelectionKey.OP_ACCEPT, null);
    
	Thread client_thread = new Thread() {
	    public void run()
	    {
	      try
		{
		  Socket s = new Socket(InetAddress.getLocalHost(), testPort);
		  OutputStream o = s.getOutputStream();
		  InputStream i = s.getInputStream();
		  int val;

		  o.write(12345678); 
		  val = i.read();		  
		}
	      catch (Exception _)
		{
		}
	    }
	  };

	client_thread.start();

	if (sel.select(1000) == 0)
	  {
	    harness.fail("Select on accept has failed");
	    return;
	  }
	else
	  harness.check(true);

	Set keys = sel.selectedKeys();

	if (!keys.contains(ssc_key))
	  {
	    harness.fail("The set does not contain the expected key");
	    return;
	  }
	else
	  harness.check(true);

	SocketChannel sc = ssc.accept();
	
	sc.configureBlocking(false);

	SelectionKey sk = sc.register(sel, SelectionKey.OP_READ, null);
	ByteBuffer bb = ByteBuffer.allocate(1);

	if (sel.select(1000) == 0)
	  {
	    harness.fail("Select on read has failed");
	    return;
	  }
	else
	  harness.check(true);

	sc.read(bb);

	if (sel.select(100) != 0)
	  {
	    harness.fail("Select on timed out read failed");
	    return;
	  }
	else
	  harness.check(true);
	
	sk.interestOps(SelectionKey.OP_WRITE);
	if (sel.select(1000) == 0)
	  {
	    harness.fail("Select on write has failed");
	    return;
	  }
	else
	  harness.check(true);

    bb.flip();
	sc.write(bb);
	sc.close();
	ssc.close();
      }
    catch (Exception e)
      {
	harness.fail("Unexpected exception " + e);
	harness.debug(e);
      }

    t.interrupt();
  }
}

