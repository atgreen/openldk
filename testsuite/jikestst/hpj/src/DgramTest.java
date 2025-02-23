// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


import java.net.*;
import java.lang.*;
import java.io.*;

class DgramTest {
 
  static Dgram2Thread dgram2_t = null;
  static Dgram1Thread dgram1_t = null;
  static int dgram2RC = -1;
  static int dgram1RC = -1;

public static void main( String argv[] ) {

  try {

    // start dgram1 first
    dgram1_t = new Dgram1Thread(null);
    dgram1_t.start();
    // start dgram2 next
    dgram2_t = new Dgram2Thread(null);
    dgram2_t.start();

    dgram2_t.join(10000 /*milli-secs*/);  // wait for dgram2 to return
    dgram1_t.join(10000 /*milli-secs*/);  // wait for dgram1 to return
    if (dgram2RC==0 && dgram1RC==0)  {
       System.out.println(0);
       System.exit(0);
    }
    else {
       System.out.println(1);
       System.exit(1);
    }
  } 
  catch( Exception e ) {
     System.out.println("Exception in DgramTest" + e);
     System.out.println(1);
     System.exit(1);
  }

  return;
} /*main*/

}

