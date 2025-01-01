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
// ******************************************************
//
// ******************************************************

package gnu.testlet.java.net.DatagramPacket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;


public class DatagramPacketTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
		byte[] b = {(byte)'a',(byte)'b',(byte)'c',(byte)'d',(byte)'e',(byte)'f',(byte)'g',(byte)'h',(byte)'i',(byte)'j',(byte)'k', (byte)'l'};

		harness.checkPoint ("test_Basics");
		DatagramPacket packet = new DatagramPacket(b, 10);

		harness.check (packet.getAddress(), null,
			"Error : test_Basics failed - 1 " + 
			"The getAddress should return null since no address is assigned yet");

		harness.check (packet.getLength(), 10,
			"Error : test_Basics failed - 2 " + 
			"The getLength should return the number of bytes to be sent/received");

		String str = new String( packet.getData());
		harness.check ( str, "abcdefghijkl",
			"Error : test_Basics failed - 3 " + 
			"The getData should return actual bytes to be sent/received");


		packet.setPort( 1000 );	
		harness.check ( packet.getPort(), 1000,
			"Error : test_Basics failed - 4 " + 
			"The getPort should return actual port to which it is set");

		packet.setLength( 3 );

		harness.check ( packet.getLength(), 3,
			"Error : test_Basics failed - 5 " + 
			"The getLength should return the number of bytes to be sent/received");

		byte b1[] = {(byte)'h' ,(byte)'h' , (byte)'i' , (byte)'j'};
		packet.setData( b1 );
		String str1 = new String( packet.getData());
		harness.check ( str1, "hhij",
			"Error : test_Basics failed - 6 " + 
			"The getData should return actual bytes to be sent/received");

		InetAddress addr = null;

		try {
			addr = 	InetAddress.getLocalHost();
			harness.check(true);
		}
		catch ( UnknownHostException e ){
			harness.fail("Error : test_Basics failed - 7 " + 
			     "The getLocalHost should not raise UnknownHostException in this case");
		}
		packet.setAddress( addr );
		harness.check ( packet.getAddress(), addr,
			"Error : test_Basics failed - 8 " + 
			"The getAddress should return the value that is assigned to it");
			
	}

	public void test_Basics1()
	{
		byte[] b = {(byte)'a',(byte)'b',(byte)'c',(byte)'d',(byte)'e',(byte)'f',(byte)'g',(byte)'h',(byte)'i',(byte)'j',(byte)'k', (byte)'l'};
		
		harness.checkPoint("test_Basics1");
		InetAddress addr0 = null;
		try {
			addr0 = 	InetAddress.getLocalHost();
			harness.check(true);
		}
		catch ( UnknownHostException e ){
			harness.fail("Error : test_Basics1 failed - 0 " + 
			     "The getLocalHost should not raise UnknownHostException in this case");
		}
		
		DatagramPacket packet = new DatagramPacket( b, 10, addr0 , 2000 );
		
		harness.check ( packet.getAddress() != null,
			"Error : test_Basics1 failed - 1 " + 
			"The getAddress should return not return null since address is assigned");

		harness.check ( packet.getLength() == 10 && packet.getPort() == 2000,
			"Error : test_Basics1 failed - 2 " + 
			"The getLength and getPort should return the number of bytes to be sent/receive" + 
			" and the port set respectively" );

		String str = new String( packet.getData());
		harness.check ( str, "abcdefghijkl",
			"Error : test_Basics1 failed - 3 " + 
			"The getData should return actual bytes to be sent/received");


		packet.setPort( 1000 );	
		harness.check ( packet.getPort(), 1000,
			"Error : test_Basics1 failed - 4 " + 
			"The getPort should return actual port to which it is set");

		packet.setLength( 3 );

		harness.check ( packet.getLength(), 3,
			"Error : test_Basics1 failed - 5 " + 
			"The getLength should return the number of bytes to be sent/received");

		byte b1[] = {(byte)'h' ,(byte)'h' , (byte)'i' , (byte)'j'};
		packet.setData( b1 );
		String str1 = new String( packet.getData());
		harness.check ( str1, "hhij",
			"Error : test_Basics1 failed - 6 " + 
			"The getData should return actual bytes to be sent/received");

		InetAddress addr = null;

		try {
			addr = 	InetAddress.getLocalHost();
			harness.check(true);
		}
		catch ( UnknownHostException e ){
			harness.fail("Error : test_Basics1 failed - 7 " + 
			     "The getLocalHost should not raise UnknownHostException in this case");
		}
		packet.setAddress( addr );
		harness.check ( packet.getAddress(), addr,
			"Error : test_Basics1 failed - 8 " + 
			"The getAddress should return the value that is assigned to it");
	}

	public void testall()
	{
		test_Basics();
		test_Basics1();
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
