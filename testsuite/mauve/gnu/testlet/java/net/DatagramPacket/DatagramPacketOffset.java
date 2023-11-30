// Tags: JDK1.2

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
 * Tests the offset-field of DatagramPacket   
 * by sending and receiving Packets
 *  
 */
public class DatagramPacketOffset implements Testlet {
        
        protected static TestHarness harness;
        
        
        public void test (TestHarness the_harness)
        {
                harness = the_harness;
                new OffsetTest();
          
        }       
        
        // +++++++++++++ first test (A)+++++++++++++++
        
        class OffsetTest {

                static final String TESTNAME = "DatagramPacket Offset Test";

                DatagramSocket localUdpSock;
                DatagramSocket localUdpSock2;

                protected DatagramPacket receivePacket;
    
                protected byte[] receiveBuf = new byte[37];

                Exception sendTextEx1;

                Exception sendTextEx2;

                public OffsetTest() {
                        
                        
                        
                        try {
                                start();
                        } catch (Exception ex) {
                                harness.fail(TESTNAME + " " + ex);
                        }
                
                        sendBadOffset();
                
                }
                
                
                void sendBadOffset() {  // runns after the other tests
                        
                        InetSocketAddress toSocketAddr = new InetSocketAddress("127.0.0.1", 4580);
                        
                        String s1 = "xyz Hello World 1234567890 ++++++++++++++";
                        // try to send packet with incorrect offset/length
                        try {
                                DatagramPacket sendPacket;
                        
                                byte[] ba1 = s1.getBytes();
                                sendPacket = new DatagramPacket(ba1, 4, ba1.length, toSocketAddr);
                                localUdpSock2.send(sendPacket);
                                                        
                                harness.check(false, "Invalid send offset/length (4/37) test (no Exception)");                                                                          
                                
                        } catch (Exception ex) {
                                harness.check(ex instanceof IllegalArgumentException, "Invalid send offset/length (4/37) test: Exception= " + ex);

                        }
                        
                        // try to send another packet with incorrect offset/length
                        try {
                                DatagramPacket sendPacket;
                        
                                byte[] ba1 = s1.getBytes();
                                sendPacket = new DatagramPacket(ba1, 40, 2, toSocketAddr);
                                localUdpSock2.send(sendPacket);
                                                        
                                harness.check(false, "Invalid send offset/length (40/2) test (no Exception)");                                                                          
                                
                        } catch (Exception ex) {
                                harness.check(ex instanceof IllegalArgumentException, "Invalid send offset/length (40/2) test: Exception= " + ex);
                                
                        }
                        
                        
                        
                }
                
                
                void start() throws Exception {
                
                        localUdpSock = new DatagramSocket(4580);
                
                        // try this for a different send-socket
                        localUdpSock2 = new DatagramSocket(4581);
                
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
        
        
                        InetSocketAddress toSocketAddr = new InetSocketAddress("127.0.0.1", 4580);


                        // send packets with offset 4           
                        int count = 0;
                
                        while (count < 10) {
                        
                                DatagramPacket sendPacket;
                        
                                String s1 = "xyz Hello World 1234567890 ++++++++++++++";
                                byte[] ba1 = s1.getBytes();
                                sendPacket = new DatagramPacket(ba1, 4, ba1.length-4, toSocketAddr);
                                localUdpSock2.send(sendPacket);
                        
                                Thread.sleep(10);
                        
                                count++;
                        
                        }
                
                        
                }

                void receiveLoop() throws Exception {
                
                
                        for (int i=0;i<receiveBuf.length;i++) {
                                receiveBuf[i] = (byte)'.';
                        }
                        // test with invalid offset/length
                        try {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 40, 3);
                                localUdpSock.receive(receivePacket);
                                harness.check(false, "Invalid receive offset/length (40/3) test failed (no Exception)");                                                                                
                        } catch (Exception ex) {
                                harness.check(ex instanceof IllegalArgumentException, "Invalid receive offset/length (40/3) test: Exception= " + ex);
                        }       

                        // test with invalid offset/test
                        try {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 20, 20);
                                localUdpSock.receive(receivePacket);
                                harness.check(false, "Invalid receive offset/length (20/20) test failed (no Exception)");                                                                               
                        } catch (Exception ex) {
                                harness.check(ex instanceof IllegalArgumentException, "Invalid receive offset/length (20/20) test: Exception= " + ex);
                        }       

                
                        // tests with correct offset            
                        {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 36, 0);
                                localUdpSock.receive(receivePacket);
                                harness.debug("after receive 0: byte[]='" + new String(receiveBuf) + "'");                                                                              
                        }               
                
                        {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 0, 10);
                                localUdpSock.receive(receivePacket);
                                harness.debug("after receive 1: byte[]='" + new String(receiveBuf) + "'");                                                                              
                        }
                        
                        {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 10, 10);
                                localUdpSock.receive(receivePacket);
                                harness.debug("after receive 2: byte[]='" + new String(receiveBuf) + "'");                                                                              
                        }
                        
                        {
                                DatagramPacket packet = receivePacket = new DatagramPacket(receiveBuf, 20, 17);
                                localUdpSock.receive(receivePacket);
                                harness.debug("after receive 3: byte[]='" + new String(receiveBuf) + "'");                                                                              
                        }

                        String result = new String(receiveBuf); 
                        String shouldBe = "Hello WorlHello WorlHello World 12345";
                        
                        harness.check(result.equals(shouldBe), 
                                TESTNAME + " byte[]='" + result + "' should be='" + shouldBe + "'"); 
                } 

        }
}
