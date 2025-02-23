// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class checkarray {
  static Truck t[][];
  static Object x[][];
  static PrintStream pp;
  public static void main(String aa[]) throws IOException {
     t = new Truck[3][4];
     x = new Object[4][5];
     pp = new PrintStream(new FileOutputStream("checkarray.out"));
     pp.println("before");
     foo();
     pp.println("after");
     int i[];
     i = (int[])x[1][2];
     pp.println("dim i" + i.length);
     pp.println(i[3]);
  }
  static void foo() throws IOException {
    Object myx;
    t[1][2] = new Truck();
    t[1][1] = new Pickup();

    t[2] = new Truck[3];
    pp.println("first");
    x[1] = new int[3][5];
    pp.println("second");
    pp.println(x[1].length);
    int ia[];
    ia = (int[])x[1][2];
    pp.println("third");
    ia[3] = 4;
    x[2] = new int[4][5][3];
    pp.println("fourth");
    myx = x;
    pp.println("fifth");
    if (! (myx instanceof Object[]) ) {
    System.out.println(4);
    System.exit(4);
    }
    try {
       ((Object[])myx)[3] = new int[9];
       System.out.println(3);
       System.exit(3);
    }
    catch (ArrayStoreException e) {
      pp.println("in catch");
    }
    pp.println("sixth");
    x[3][1] = new Truck();
    pp.println("seventh");
    System.out.println(5);
    System.exit(5);
  }
}

     
