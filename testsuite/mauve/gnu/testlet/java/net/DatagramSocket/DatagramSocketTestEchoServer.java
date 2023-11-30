// Tags: not-a-test

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
import gnu.testlet.TestHarness;
import java.net.*;
import java.io.*;

// mod for "equals" and remove dependency on NodeTest

class DatagramSocketTestEchoServer extends Thread {

  protected static TestHarness harness;

    public void run() {
		try {
		DatagramSocket sock = new DatagramSocket( 8000 );

		byte[] inbuf = new byte[10];
		DatagramPacket request = new DatagramPacket(inbuf, inbuf.length);
		try {
			{
//				System.out.println("       Server: Wait for Receive request");
				sock.receive(request);
//				System.out.println("       Server: Request received");
				
				DatagramPacket pack = new DatagramPacket( inbuf , 10, 
					                        InetAddress.getLocalHost() , 
					
											request.getPort());
				
//				System.out.println("       Server: Sending packet back "+ "Data: "+(new String(pack.getData())));
				
				sock.send(pack); 
//				System.out.println("       Server: Packet sent back");
			}
		} catch (IOException e) {
			System.out.println("Server: run failed with IOException " );
			e.printStackTrace();
		}

		}catch ( Exception e )
		{
			System.out.println("Server: run failed with exception " );
			e.printStackTrace();
		}
    }
}
