// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.net.*;
import java.lang.*;

class dgram2 {

public static void main( String argv[] ) {

  String remote_host = argv[0];
  //String remote_host = "steve2.watson.ibm.com";
  int    remote_port = 5566;
  int    local_port  = 6655;

  InetAddress ia;
  DatagramPacket p,p2;
  DatagramSocket s;
  int msglen;
  byte[] sendbytes,recvbytes;
  String recvmsg;
  String sendmsg;

  try {
    ia = InetAddress.getByName( remote_host );
    sendbytes = new byte[64];
    recvbytes = new byte[512];
    s = new DatagramSocket( local_port );

    System.out.println("DGRAM2: receiving on port " + local_port);
    System.out.println("DGRAM2: sending to " + remote_host
                                 + " at port " + remote_port);

    for (int i = 1; i<=10; i++) {
        sendmsg = "DGRAM2 message " + i;
        msglen = sendmsg.length();
        sendmsg.getBytes(0,msglen,sendbytes,0);
        p = new DatagramPacket( sendbytes, msglen, ia, remote_port );
        System.out.println("DGRAM2: sending msg = <" + sendmsg + ">");
        s.send(p);

        p2 = new DatagramPacket(recvbytes,recvbytes.length);
        s.receive( p2 );
        recvmsg = new String(recvbytes,0);
        System.out.println("DGRAM2: received msg = <" + recvmsg + ">");
    }
  }
  catch( Exception e ) {
     System.out.println("Exception in dgram2"+e);
  }

} /*main*/

}

