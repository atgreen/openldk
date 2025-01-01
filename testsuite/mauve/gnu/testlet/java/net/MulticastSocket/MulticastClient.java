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

package gnu.testlet.java.net.MulticastSocket;
import gnu.testlet.TestHarness;
import java.net.*;


class MulticastClient extends Thread {

  protected static TestHarness harness;

    public MulticastClient() {
	try {
                        socket = new MulticastSocket(4446);
                        address = InetAddress.getByName("230.0.0.1");

                        socket.joinGroup(address);
                        clientPort = socket.getLocalPort();
	}catch(Exception e){
			System.out.println("Client constructor failed");
			e.printStackTrace();
	}
    }

    public void run() {
	// System.out.println("Starting Client");
		try {
		    for (;;) {
                                byte[] buf = new byte[256];
                                packet = new DatagramPacket(buf, buf.length);
                                socket.receive(packet);

                                String received = new String(packet.getData());
                                //System.out.println("Received: " + received);
                                if(received.startsWith("bye"))
                                        break;
		    }
                        socket.leaveGroup(address);
                        socket.close();

                }catch(Exception e){
			System.out.println("Client run failed");
			e.printStackTrace();
		}
    }

    public int getPort() {
                return clientPort;
    }
private int clientPort;
private MulticastSocket socket;
private InetAddress address;
private DatagramPacket packet;
}
