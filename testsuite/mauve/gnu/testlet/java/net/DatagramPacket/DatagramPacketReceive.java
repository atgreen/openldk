// Tags: JDK1.0

/*
   Copyright (C) 2003 Norbert Frese

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

/**
 * 
 * Tests the 'reusing' of a DatagramPacket-Object for receiving
 * multiple Packets.  
 *  
 */
public class DatagramPacketReceive implements Testlet {
        
        protected static TestHarness harness;
        
        
        
        
        public void test (TestHarness the_harness)
        {
                harness = the_harness;
                new UDPTest1();
          
        }       
        
        // +++++++++++++ first test (A)+++++++++++++++
        
        class UDPTest1 {

                static final String TESTNAME = "DatagramPacket receive Test A";

                DatagramSocket localUdpSock;
                DatagramSocket localUdpSock2;

                protected DatagramPacket receivePacket;
    
                protected byte[] receiveBuf = new byte[1000];


                public UDPTest1() {
                        
                        
                        
                        try {
                                start();
                        } catch (Exception ex) {
                                harness.fail(TESTNAME + " " + ex);
                        }
                
                
                }
                
                void start() throws Exception {
                
                        localUdpSock = new DatagramSocket(4567);
                
                        // try this for a different send-socket
                        localUdpSock2 = new DatagramSocket(4568);
                
                        // or this for using the same socket for sending and receiving:
                        //localUdpSock2 = localUdpSock;
                
                        Thread sendThread = new Thread(new Runnable() {

                                public void run() {
                                        try {
                                                sendLoop();
                                        } catch (Exception ex) {
                                                ex.printStackTrace();
                                        }
                                
                                }
                        
                        });
                
                        sendThread.start();
                        receiveLoop();
                
                }
        
        
                void sendLoop() throws Exception {
        
        
                        InetSocketAddress toSocketAddr = new InetSocketAddress("127.0.0.1", 4567);
                
                        int count = 0;
                
                        while (count < 10) {
                        
                                DatagramPacket sendPacket;
                        
                                String s1 = "Hello World 1234567890 ++++++++++++++";
                                byte[] ba1 = s1.getBytes();
                                sendPacket = new DatagramPacket(ba1, ba1.length, toSocketAddr);
                                localUdpSock2.send(sendPacket);
                        
                                String s2 = "World Hello 0987654321";
                                byte[] ba2 = s2.getBytes();
                                sendPacket = new DatagramPacket(ba2, ba2.length, toSocketAddr);
                                localUdpSock2.send(sendPacket);
                        
                                Thread.sleep(10);
                        
                                count++;
                        
                        }
                
                        
                }

                void receiveLoop() throws Exception {
                
                        receivePacket = new DatagramPacket(receiveBuf, receiveBuf.length);
                
                        int count=0;
                        int errorCount=0;               
                
                        while(count < 10) {
                                try {
                                        
                                                localUdpSock.receive(receivePacket);
                                                Thread.sleep(5);
                                                int reportedLength = receivePacket.getLength();
                                        
                                                String s = new String(receiveBuf, 0, reportedLength);

                                                String message = "packet#" + count + " got packet '" + s + "' length=" + receivePacket.getLength();
                                                harness.debug(message);                                          
                                                
                                                if (!(
                                                                (s.startsWith("World Hello") && reportedLength == 22) 
                                                                || (s.startsWith("Hello World") && reportedLength == 37)
                                                )) errorCount++;
                                                                
                                } catch (Exception ex) {
                                        harness.fail(TESTNAME + " receiveloop exception:" + ex);
                                }
                                count++;
                                
                        }
                        
                        harness.check(errorCount ==0, 
                                TESTNAME + " errorCount=" + errorCount); 
                } 

        }
}
