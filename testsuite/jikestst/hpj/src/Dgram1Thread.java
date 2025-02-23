// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.net.*;
import java.lang.*;

class Dgram1Thread extends Thread {

  String hostname = null;

  Dgram1Thread( String host /* null for self/localhost */ ) {
     hostname = host;
  }

public void run() {

  int    remote_port = 6655;
  int    local_port  = 5566;

  InetAddress ia;
  DatagramPacket p,p2;
  DatagramSocket s;
  int msglen;
  byte[] sendbytes,recvbytes,recvdata;
  String recvmsg;
  String sendmsg;

  try {
    ia = InetAddress.getByName( hostname );
    sendbytes = new byte[512];
    recvbytes = new byte[64];
    s = new DatagramSocket( local_port );

    for (int i = 1; i<=10; i++ ) {
        p2 = new DatagramPacket(recvbytes,recvbytes.length);
        s.receive( p2 );
        recvdata = p2.getData();
        recvmsg = new String(recvdata,0,0,p2.getLength());
        if ( ! recvmsg.equals("DGRAM2 "+i)) {DgramTest.dgram1RC=1;return;}

        sendmsg = "DGRAM1 " + i;
        msglen = sendmsg.length();
        sendmsg.getBytes(0,msglen,sendbytes,0);
        p = new DatagramPacket( sendbytes, msglen, ia, remote_port );
        s.send(p);
    }
   }
   catch( Exception e ) {
     System.out.println("Exception in Dgram1Thread"+e);
     DgramTest.dgram1RC = 1;
     return;
  }
  DgramTest.dgram1RC = 0;
  return;

} /*run*/

}

