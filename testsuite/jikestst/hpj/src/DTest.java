// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class Test {
  public static void main(String args[])
   throws IOException {
    java.util.Date d = new java.util.Date(1000000000000L);
    java.lang.String s;
    FileOutputStream f = new FileOutputStream("Dates.out");
    PrintStream o = new PrintStream(f);
    s = d.toString();
    o.println(s);
    s = d.toLocaleString();
    o.println(s);
    s = d.toGMTString();
    o.println(s);
    System.out.println(s);
    System.exit(0);
  }
}
