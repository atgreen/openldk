// Tags: JDK1.1
// Uses: DatagramSocketTestEchoServer DatagramSocketTestEchoTimeoutServer 

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

package gnu.testlet.java.net.DatagramSocket;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;

// mod for "equals" and remove dependency on NodeTest

public class DatagramSocketTest implements Testlet
{
  protected static TestHarness harness;
	public void test_Basics()
	{
		harness.checkPoint("Basics");
		try {
			DatagramSocket 	sock1 = new DatagramSocket( 7000 );
			harness.check ( sock1.getLocalPort(), 7000,
				"Error : test_Basics failed - 1 " + 
				"returned port value is wrong");

			
		}
		catch( SocketException e ){
			harness.fail("Error : test_Basics failed - 2 " + 
				"Not able to create a socket ");
		}
		catch(IllegalArgumentException e )
		{
			harness.fail("Error : test_Basics failed - 3a " + 
				"Port 7000 causes IllegalArgumentException");
		}
		catch(Exception e )
		{
			harness.fail("Error : test_Basics failed - 3b ");
			harness.debug(e);
		}

		try {
			DatagramSocket sock3 =
				new DatagramSocket( 7001,
					InetAddress.getLocalHost());
			
			harness.check (sock3.getLocalAddress().getHostAddress(),
				InetAddress.getLocalHost().getHostAddress (),
				"Error : test_Basics failed - 4 " + 
					 "ip address returned is not correct ");
		}
		catch ( SocketException e ){
			harness.fail("Error : test_Basics failed - 5 " + 
				"Not able to create a socket ");
		}
		catch ( UnknownHostException e ){
			harness.fail("Error : test_Basics failed - 6 " + 
				"Should not throw UnknownHostException ");
		}
		catch(IllegalArgumentException e )
		{
			harness.fail("Error : test_Basics failed - 7 " + 
				"Port 7001 causes IllegalArgumentException");
		}
		catch(Exception e )
		{
			harness.fail("Error : test_Basics failed - 8 ");
			harness.debug(e);
		}

	}


	public void test_echo()
	{
		DatagramSocketTestEchoServer srv = new DatagramSocketTestEchoServer();
		srv.setDaemon(true);
		srv.setPriority(10);
		srv.start();
		
		
	//	System.out.println(" yield to server thread");
		Thread.yield();
		try {
			Thread.sleep(2000);
		}
		catch ( Exception e ){}
	//	System.out.println(" server thread should be running");
		
		byte buff[] = {(byte)'h' , (byte)'e', (byte)'l',(byte)'l',(byte)'o',(byte)'b',(byte)'u',(byte)'d',(byte)'d',(byte)'y'};

		
		DatagramSocket client=null;
		DatagramPacket request=null;
		
		try {
			client = new DatagramSocket(); 
			request = new DatagramPacket( 
							buff, buff.length, 
							InetAddress.getLocalHost(), 8000 );
//			System.out.println("Packet Addressed to :"+InetAddress.getLocalHost().toString()+" "+String.valueOf(8000));
			harness.check(true);
		}
		catch ( Exception e ){
			harness.fail("Error : test_echo failed - 0 " + 
				"Should not throw Exception ");	
		}

		byte resp[] = new byte[10];
		DatagramPacket reply = new DatagramPacket( resp , resp.length );
		
		if (client==null) return;
		if (request==null) return;
		
//		System.out.println("test echo 1");
		
		try {
//			System.out.println("test echo 2 send");
			client.send(request);
			try {
				Thread.sleep(1000 );
			}
			catch ( Exception e ){}
//			System.out.println("test echo 3 receive");
			client.receive(reply);
//			System.out.println("test echo 4 received, close.");
			client.close();

			//try {
			//System.out.println("test echo 4.5 close 2");
			//	client.close();
			//	harness.fail("Error : test_echo failed - 1 " + 
			//		"IOException to be thrown if a socket is closed twice ");
			//}
			//catch(  Exception e ){}
			
//			System.out.println("test echo 5");

			try {
				byte resp1[] = new byte[10];
				DatagramPacket reply1 = new DatagramPacket( resp1 , resp1.length );
//				System.out.println("test echo 6");
				
				client.receive(reply1);
				harness.fail( "Error : test_echo failed - 2 " + 
					"IOException should be thrown if try to read after the socket is closed");
			}
			catch ( IOException e )
			{
				harness.check(true);
			}

		}
		catch ( Exception e)
		{
			harness.fail("Error : test_echo failed - 3 " + 
				    "Exception occured while sending/receiving " );
		}
						
		harness.check ( reply.getLength(), 10,
			"Error : test_echo failed - 4 " + 
			"server did not return proper number of bytes " );
			
//		System.out.println("test_echo: packet data: "+(new String(reply.getData()))  ) ;
			

		harness.check ( (new String(reply.getData())), "hellobuddy",
			"Error : test_echo - 5 failed " + 
			"The echo server did not send the expected data " );

	}

// 12/16/97, timeout is not a Java 1.0 feature 

	public void test_echoWithTimeout()
	{
		DatagramSocketTestEchoTimeoutServer srv = new DatagramSocketTestEchoTimeoutServer();
		srv.setDaemon(true);
		srv.setPriority(10);
		srv.start();
		Thread.yield();
		try {
			Thread.sleep(2000);
		}
		catch ( Exception e ){}
		
		byte buff[] = {(byte)'h' , (byte)'e', (byte)'l',(byte)'l',(byte)'o',(byte)'b',(byte)'u',(byte)'d',(byte)'d',(byte)'y'};

		
		DatagramSocket client=null;
		DatagramPacket request=null;
		
		try {
			client = new DatagramSocket(); 
			request = new DatagramPacket( 
							buff, buff.length, 
							InetAddress.getLocalHost(), 8001 );
			harness.check(true);
		}
		catch ( Exception e ){
			harness.fail("Error : test_echoWithTimeout failed - 0 " + 
				"Should not throw Exception ");	
		}
		
		if (client==null) return;
		if (request==null) return;
		
		try {
			client.setSoTimeout(500);
			harness.check(true);
		}catch ( SocketException e ){
			harness.fail("Error : test_echoWithTimeout failed - 1 " + 
				"Should not throw SocketException ");				
		}

		try {
			harness.check ( client.getSoTimeout(), 500,
				"Error : test_echoWithTimeout failed - 2 " + 
					"did not return proper timeout value ");
		}catch ( SocketException e ){
			harness.fail("Error : test_echoWithTimeout failed - 3 " + 
				"Should not throw SocketException ");				
		}


		byte resp[] = new byte[10];
		DatagramPacket reply = new DatagramPacket( resp , resp.length );
		
		try {
			client.send(request);
			client.receive(reply);
		  // don't send data & test that 
		  // receive times-out
			try {
			
			  client.setSoTimeout(1);
			  client.receive(reply);
				harness.fail("Error : test_echoWithTimeout failed - 2 " +
					"Should throw interrupted exception after the specified duration");
			} catch (InterruptedIOException e )
			{
				harness.check(true);
			} catch (IOException ioe) 
			{
				harness.debug(ioe);
				harness.check(false);
			}
			client.close();

			try {
				byte resp1[] = new byte[10];
				DatagramPacket reply1 = new DatagramPacket( resp1 , resp1.length );
				client.receive(reply1);
				harness.fail( "Error : test_echoWithTimeout failed - 4 " + 
					"IOException should be thrown if try to read after the socket is closed");
			}
			catch ( IOException e )
			{
				harness.check(true);
			}

		}
		catch ( Exception e)
		{
			harness.fail("Error : test_echoWithTimeout failed - 5 " + 
				    "Exception occured while sending/receiving " );
			harness.debug(e);
		}
						
		harness.check ( reply.getLength(), 10,
			"Error : test_echoWithTimeout failed - 6 " + 
			"server did not return proper number of bytes " );
			
//		System.out.println("test_echoWithTimeout: packet data: "+(new String(reply.getData()))  ) ;

		harness.check ( (new String(reply.getData())), "hellobuddy",
			"Error : test_echoWithTimeout - 7 failed " + 
			"The echo server didnot send the expected data " );

	}


	public void testall()
	{
//		System.out.println("test_basics");
		test_Basics();
//		System.out.println("test_echo");
		test_echo();	
		test_echoWithTimeout();
//		System.out.println("testall Done 1");
	}

  public void test (TestHarness the_harness)
  {
    harness = the_harness;
    testall ();
  }

}
