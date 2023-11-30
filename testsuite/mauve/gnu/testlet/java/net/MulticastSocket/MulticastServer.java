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


class MulticastServer extends Thread {

  protected static TestHarness harness;
  
  private int serverPort;
  private MulticastSocket socket;
  private InetAddress address;
  private DatagramPacket packet;

  public MulticastServer(int nPort) {
    try {
      serverPort = nPort;
      socket = new MulticastSocket();
      address = InetAddress.getByName("230.0.0.1");
    } 
    catch (Exception e) {
      System.out.println("Server constructor");
      e.printStackTrace();
    }
  }
  
  public void run() {
    //System.out.println("Starting Server");
    
    
    try {
      String[] cmd = new String[5];
      
      cmd[0] = "hello";
      cmd[1] = "there";
      cmd[2] = "this is";
      cmd[3] = "multicast";
      cmd[4] = "bye";
      
      for(int i = 0; i < 5; i++){
        packet = new DatagramPacket(cmd[i].getBytes(), cmd[i].length(),
                                    address, serverPort);
        // System.out.println("Sent: " + cmd[i]);
        socket.send(packet);
      }
      socket.close();
    } 
    catch (Exception e) {
      System.out.println("Server run failed");
      e.printStackTrace();
    }
  }
  
}
