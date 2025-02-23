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
    FileOutputStream fos = new FileOutputStream("FOSTest.out");
    PrintStream ps = new PrintStream(fos);

    ps.println("Hello World");
    ps.close();
    System.out.println(0);
    System.exit(0);
  }
}
