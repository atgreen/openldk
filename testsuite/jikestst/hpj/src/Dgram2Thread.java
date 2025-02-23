// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.net.*;
import java.lang.*;

class Dgram2Thread extends Thread {

  String hostname = null;

  Dgram2Thread( String host /* null for self/localhost */ ) {
     hostname = host;
  }

public void run() {

  int    remote_port = 5566;
  int    local_port  = 6655;

  InetAddress ia;
  DatagramPacket p,p2;
  DatagramSocket s;
  int msglen;
  byte[] sendbytes,recvbytes;
  String recvmsg;
  String sendmsg;
  String expectedmsg;

  try {
    ia = InetAddress.getByName( hostname );
    sendbytes = new byte[64];
    recvbytes = new byte[512];
    s = new DatagramSocket( local_port );

    for (int i = 1; i<=10; i++) {
        sendmsg = "DGRAM2 " + i;
        msglen = sendmsg.length();
        sendmsg.getBytes(0,msglen,sendbytes,0);
        p = new DatagramPacket( sendbytes, msglen, ia, remote_port );
        s.send(p);

        p2 = new DatagramPacket(recvbytes,recvbytes.length);
        s.receive( p2 );
        recvmsg = new String(recvbytes,0,0,p2.getLength());
        expectedmsg = "DGRAM1 "+i;

        if ( ! recvmsg.equals(expectedmsg)) {
           DgramTest.dgram2RC=1;
           return;
        }
    }
  }
  catch( Exception e ) {
     DgramTest.dgram2RC = 1;
     return;
  }
  DgramTest.dgram2RC = 0;
  return;

} /*run*/

}

