// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.net.*;
import java.lang.*;
import java.io.*;

class clientsock {

public static void main( String argv[] ) {

  try {

    InetAddress server_ia = InetAddress.getByName( null );
    Socket s = new Socket( server_ia, 6655 );
    if ( s==null ) {
       System.out.println("socket returned null");
       System.exit(1);
    }
    int localPort = s.getLocalPort();
    int remotePort = s.getPort();
    System.out.println("Client: have Socket: localPort=" + localPort
                           + " remotePort=" + remotePort);

    InetAddress ia = s.getInetAddress();
    System.out.println("Client: connected to InetAddress = " + ia );

    OutputStream os = s.getOutputStream();
    PrintStream ps = new PrintStream(os);

    ps.println("Hello World");
    ps.println("Disney World");
    ps.println("Web Wide World");
    ps.println("Goodbye Cruel World");
    ps.close();

    InputStream is = s.getInputStream();

    // add more here later
   
  } 
  catch( Exception e ) {
     System.out.println("Exception in clientsock" + e);
  }

  System.exit(0);
} /*main*/

}

