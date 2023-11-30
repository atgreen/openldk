// Tags: JDK1.1

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
*  File name: DatagramPacketTest2.java
**********************************************/

package gnu.testlet.java.net.DatagramPacket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;

/**************************************************************
*
* What does the test do?
* ----------------------
*
* This test is for DatagramPacket class.
* It tests for Exceptions, by passing invalid arguments to
* the constructors and methods.
* 
* How do I run the test?
* ----------------------
*
* Usage: java DatagramPacketTest2
*
* What about the test result?
* ---------------------------
*
* If an Exception is not thrown, when it should have been,
* and vice-versa, then the error is displayed on stdout and
* the test continues.
*
* Check the file Test.out for any errors.
*
**************************************************************/

public class DatagramPacketTest2 implements Testlet
{
	final static int INVALID_PORT = -1;
	final static int PORT = 7;
	final static int MAX_PORT = 65535;

  protected static TestHarness harness;
	public InetAddress ia;
	public byte [] buf;

	public DatagramPacketTest2() throws Exception
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

	// check for invalid data buffer
	public void invalid_buf()
	{
		try
		{
			DatagramPacket request = new DatagramPacket(null, 10, ia, PORT);
			errormsg("invalid_buf", 1, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket request = new DatagramPacket(buf, buf.length, ia,
			                                                          PORT);
			request.setData(null);
			errormsg("invalid_buf", 2, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(null, 10);
			errormsg("invalid_buf", 3, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, buf.length);
			reply.setData(null);
			errormsg("invalid_buf", 4, true, "NullPointerException");
		}
		catch (NullPointerException e)
		{
			harness.check(true);
		}
	}

	// check for invalid data buffer length
	public void invalid_buflen()
	{
		try
		{
			DatagramPacket request = new DatagramPacket(buf, -1, ia, PORT);
			errormsg("invalid_buflen", 1, false, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket request = new DatagramPacket(buf, 0, ia, PORT);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_buflen", 2, true, null);
			e.printStackTrace();
		}

		try
		{

			DatagramPacket reply = new DatagramPacket(buf, -1);
			errormsg("invalid_buflen", 3, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, 0);
			harness.check(true);
		}
		catch (Exception e)
		{
			errormsg("invalid_buflen", 4, true, null);
			e.printStackTrace();
		}

		try
		{
			DatagramPacket request = new DatagramPacket(buf, buf.length + 1,
			                                                      ia, PORT);
			errormsg("invalid_buflen", 5, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, buf.length + 1);
			errormsg("invalid_buflen", 6, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, buf.length);
			reply.setLength(buf.length + 1);
			errormsg("invalid_buflen", 7, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}
	}

	// check for invalid port number
	public void invalid_port()
	{
		try
		{
			DatagramPacket request = new DatagramPacket(buf, 10, ia,
			                                                INVALID_PORT);
			errormsg("invalid_port", 1, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket request = new DatagramPacket(buf, 10, ia, PORT);
			request.setPort(INVALID_PORT);
			errormsg("invalid_port", 2, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket request2 = new DatagramPacket(buf, 10, ia,
			                                                MAX_PORT + 1);
			errormsg("invalid_port", 3, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, 10);
			reply.setPort(INVALID_PORT);
			errormsg("invalid_port", 4, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}

		try
		{
			DatagramPacket reply = new DatagramPacket(buf, 10);
			reply.setPort(MAX_PORT + 1);
			errormsg("invalid_port", 5, true, "IllegalArgumentException");
		}
		catch (IllegalArgumentException e)
		{
			harness.check(true);
		}
	}
	
  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }


	public void testall()
	{
		DatagramPacketTest2 m = null;
		try
		{
			m = new DatagramPacketTest2();
			harness.check(true);
		}
		catch (Exception e)
		{
			harness.fail("DatagramPacketTest2 constructor");
			e.printStackTrace();
			System.exit(1);
		}

		m.invalid_buf();
		m.invalid_buflen();
		m.invalid_port();
	}
}
