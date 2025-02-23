// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class recur {
  static int i = 1;
  static PrintStream pp;
  public static void main(String argv[]) throws IOException {
       if (i==1) pp = new PrintStream(new FileOutputStream("recur.out"));
       if (i==1) fred(argv);
       pp.println("In main");
   }

  static void fred(String argv[]) throws IOException {
     pp.println("In fred");
     i = i + 1;
     main(argv);
   }

}
