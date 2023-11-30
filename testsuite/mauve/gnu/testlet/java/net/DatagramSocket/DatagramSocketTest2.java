// Tags: JDK1.0

/*
   Copyright (C) 1999 Hewlett-Packard Company

   This file is part of Mauve.

   Mauve is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Mauve is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Mauve; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/
/**********************************************
*  File name: DatagramSocketTest2.java
**********************************************/

package gnu.testlet.java.net.DatagramSocket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;
import java.net.*;

/**************************************************************
*
* What does the test do?
* ----------------------
*
* This test is for DatagramSocket class.
* It tests for Exceptions, by passing invalid arguments to
* the constructors and methods.
* 
* How do I run the test?
* ----------------------
*
* Usage: java DatagramSocketTest2
*
* What about the test result?
* ---------------------------
*
* If an Exception is not thrown, when it should have been,
* and vice-versa, then the error is displayed on stderr and
* the test continues.
*
* TEST PASS == No output on stderr
*
**************************************************************/

public class DatagramSocketTest2 implements Testlet
{
	final static int INVALID_PORT = -1;
	final static int ECHO_PORT = 7;
	final static int GOOD_PORT = 37777;
	final static int MAX_PORT = 65535;

  protected static TestHarness harness;
	public InetAddress ia;
	public byte [] buf;

	public DatagramSocketTest2() throws Exception
	{
		buf = new byte[10];
		ia = InetAddress.getLocalHost();
	}
	
	private void errormsg(String m, int num, boolean flag, String e)
	{
		if (e != null)
		{
			if (flag)
				harness.fail(m + ": " + "test " + num +
					" - Should throw " + e);
			else
				harness.fail(m + ": " + "test " + num +
					" - Should NOT throw " + e);
		}
		else
			harness.fail(m + ": " + "test " + num +
				" - Should NOT throw any Exception");
	}

	// check for invalid port number
	public void invalid_port()
	{
		harness.checkPoint("invalid_port");
		try
		{
			DatagramSocket sock = new DatagramSocket(INVALID_PORT);
			errormsg("invalid_port", 2, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_port", 2, false, "IOException");
		}

		try
		{
			DatagramSocket sock = new DatagramSocket(MAX_PORT + 1);
			errormsg("invalid_port", 3, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_port", 3, false, "IOException");
		}
	}

	// check for invalid IP address
	public void invalid_addr()
	{
		harness.checkPoint("invalid_addr");
		DatagramSocket sock = null;
		try
		{
			sock = new DatagramSocket(GOOD_PORT, null);
			harness.check(true);
		}
		catch (NullPointerException e)
		{
			errormsg("invalid_addr", 1, false, "NullPointerException");
		}
		catch (IOException e)
		{
			errormsg("invalid_addr", 1, false, "IOException");
		}

		if (sock != null)
			sock.close();
	}

	// check for invalid data buffer in receive packet
	public void invalid_receive_data()
	{
		harness.checkPoint("invalid_receive_data");
		DatagramSocket sock = null;
		try
		{
			sock = new DatagramSocket(GOOD_PORT, ia);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_receive_data", 1, false, "Exception");
			e.printStackTrace();
			return;
		}

		try
		{
			// null packet
			sock.receive(null);
			errormsg("invalid_receive_data", 2, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_receive_data", 2, false, "IOException");
		}

		try
		{
			DatagramPacket pkt = new DatagramPacket(buf, buf.length);
			// null data buffer
			pkt.setData(null);
			sock.receive(pkt);
			errormsg("invalid_send_data", 3, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_send_data", 3, false, "IOException");
		}

		try
		{
			// invalid data buffer length
			DatagramPacket pkt = new DatagramPacket(buf, -1);
			errormsg("invalid_receive_data", 4, true, "IOException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}
		catch (Exception e)
		{
			harness.check(false, "Expected IllegalArgumentException");
		}

		sock.close();
	}

	// check for invalid IP address in send packet
	public void invalid_send_addr()
	{
		harness.checkPoint("invalid_send_addr");
		DatagramSocket sock = null;
		try
		{
			sock = new DatagramSocket(GOOD_PORT, ia);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_send_addr", 1, false, "Exception");
			return;
		}

		try
		{
			// null IP address
			DatagramPacket pkt = new DatagramPacket(buf, buf.length, null,
			                                                 ECHO_PORT);
			sock.send(pkt);
			errormsg("invalid_send_addr", 2, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_send_addr", 2, false, "IOException");
		}

		sock.close();
	}

	// check for invalid port in send packet
	public void invalid_send_port()
	{
		harness.checkPoint("invalid_send_port");
		DatagramSocket sock = null;
		try
		{
			sock = new DatagramSocket(GOOD_PORT, ia);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_send_port", 1, false, "Exception");
			return;
		}

		try
		{
			// invalid port 0
			DatagramPacket pkt = new DatagramPacket(buf, buf.length, ia, 0);
			sock.send(pkt);
			errormsg("invalid_send_port", 2, true, "IOException");
		}
		catch (IOException e)
		{
			harness.check(true);
		}

		sock.close();
	}

	// check for invalid data buffer in send packet
	public void invalid_send_data()
	{
		harness.checkPoint("invalid_send_data");
		DatagramSocket sock = null;
		try
		{
			sock = new DatagramSocket(GOOD_PORT, ia);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_send_data", 1, false, "Exception");
			return;
		}

		try
		{
			// null packet
			sock.send(null);
			errormsg("invalid_send_data", 2, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_send_data", 2, false, "IOException");
		}

		try
		{
			DatagramPacket pkt = new DatagramPacket(buf, buf.length, ia,
			                                                    ECHO_PORT);
			// null data buffer
			pkt.setData(null);
			sock.send(pkt);
			errormsg("invalid_send_data", 3, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_send_data", 3, false, "IOException");
		}

		try
		{
			// invalid data buffer length
			DatagramPacket pkt = new DatagramPacket(buf, -1, ia, ECHO_PORT);
			errormsg("invalid_send_data", 4, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			// zero data buffer length
			DatagramPacket pkt = new DatagramPacket(buf, 0, ia, ECHO_PORT);
			sock.send(pkt);
			harness.check(true);
		}
		catch (IOException e)
		{
			errormsg("invalid_send_data", 5, false, "IOException");
		}

		sock.close();
	}
	
  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }


	public void testall()
	{
		DatagramSocketTest2 m = null;
		try
		{
			m = new DatagramSocketTest2();
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}

		m.invalid_port();
		m.invalid_addr();
		m.invalid_receive_data();
		m.invalid_send_addr();
		m.invalid_send_port();
		m.invalid_send_data();
	}
}
