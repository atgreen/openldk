// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class Test {
  public static void main(String args[])
   throws java.io.IOException {
    String s;

    // First create a data file.
    FileOutputStream fos = new FileOutputStream("FISTest.out");
    PrintStream ps = new PrintStream(fos);

    ps.println("Hello World");
    ps.println("Disney World");
    ps.println("Web Wide World");
    ps.println("Goodbye Cruel World");
    ps.close();

    // Now read it in and verify stuff.
    FileInputStream fis = new FileInputStream("FISTest.out");
    DataInputStream dis = new DataInputStream(fis);

    s = dis.readLine();
    if (!s.equals("Hello World")) {
    System.out.println(1);
    System.exit(1);
    }

    if (fis.available() != 48) {
    System.out.println(2);
    System.exit(2);
    }

    s = dis.readLine();
    if (!s.equals("Disney World")) {
    System.out.println(3);
    System.exit(3);
    }

    if (fis.skip(15) != 15) {
    System.out.println(4);
    System.exit(4);
    }

    s = dis.readLine();
    if (!s.equals("Goodbye Cruel World")) {
    System.out.println(5);
    System.exit(5);
    }

    fis.close();

    // Clean up our mess.
    File f = new File("FISTest.out");
    if (!f.delete()) {
    System.out.println(1);
    System.exit(1);
    }

    System.out.println(0);
    System.exit(0);
  }
}
