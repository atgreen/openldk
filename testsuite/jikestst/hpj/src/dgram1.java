// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.net.*;
import java.lang.*;

class dgram1 {

public static void main( String argv[] ) {

  String remote_host = argv[0];
  //String remote_host = "corinth.watson.ibm.com";
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
    ia = InetAddress.getByName( remote_host );
    sendbytes = new byte[512];
    recvbytes = new byte[64];
    s = new DatagramSocket( local_port );

    System.out.println("DGRAM1: receiving on port " + local_port);
    System.out.println("DGRAM1: sending to "
                  + remote_host + " at port " + remote_port);
    
    for (int i = 1; i<=10; i++ ) {
        p2 = new DatagramPacket(recvbytes,recvbytes.length);
        s.receive( p2 );
        recvdata = p2.getData();
        recvmsg = new String(recvdata,0,0,p2.getLength());
        System.out.println("DGRAM1: received msg = <" + recvmsg + ">");

        sendmsg = "DGRAM1 message " + i;
        msglen = sendmsg.length();
        sendmsg.getBytes(0,msglen,sendbytes,0);
        p = new DatagramPacket( sendbytes, msglen, ia, remote_port );
        System.out.println("DGRAM1: sending msg = <" + sendmsg + ">");
        s.send(p);
    }
   }
  catch( Exception e ) {
     System.out.println("Exception in dgram1"+e);
  }

} /*main*/

}

