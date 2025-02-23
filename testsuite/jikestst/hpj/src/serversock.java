// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.



import java.net.*;
import java.lang.*;
import java.io.*;

class serversock {

public static void main( String argv[] ) {

  InetAddress ia;
  Socket s;
  String msg = "A MSG FROM STEVE";

  try {
    ServerSocket ss = new ServerSocket( 6655, 10 );
    int sslocalPort = ss.getLocalPort();   
    System.out.println("Server: sslocalPort = " + sslocalPort );

    s = ss.accept();

    int slocalPort = s.getLocalPort();
    int sremotePort = s.getPort();
    System.out.println("Server: slocalPort = " + slocalPort
                              + " sremotePort = " + sremotePort );

    ia = s.getInetAddress();
    System.out.println("  s_ia (InetAddress) = " + ia );

    InputStream is = s.getInputStream();

    DataInputStream dis = new DataInputStream(is);

    String str = dis.readLine();
System.out.println("Server received <" + str + ">");
    if (!str.equals("Hello World")) System.exit(1);
   
    // following will call native socketAvailable which is not
    // implemented....ie we will have to do it 
    // ...of course, this is a stream, not a file, and it is wrong to
    // require all 48 bytes to be available.
    //
    // if (is.available() != 48) System.exit(2);

    str = dis.readLine();
System.out.println("Server received <" + str + ">");
    if (!str.equals("Disney World")) System.exit(3);

System.out.println("Server skipping 15 bytes");
    if (is.skip(15) != 15) System.exit(4);

    str = dis.readLine();
System.out.println("Server received <" + str + ">");
    if (!str.equals("Goodbye Cruel World")) System.exit(5);

    is.close();

    // add more here later
   
  } 
  catch( Exception e ) {
     System.out.println("Exception in serversock" + e);
  }

  System.exit(0);
} /*main*/

}

