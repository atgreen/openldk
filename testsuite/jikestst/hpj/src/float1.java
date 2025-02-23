// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
  static float x;
  static double d;
  public static void main(String aa[]) {
    float ff[] = new float[5];
    double dd[] = new double[4];

    x = (float)1.3;
    
    ff[1] = x;
    ff[2] = (float)3.5;

    ff[3] = f();

    dd[1] = 1;
    dd[2] = 1.3;
    dd[3] = x;
    dd[0] = f();

    x = (dd[0] == ff[3]) ? (float)4.4 : (float)5.5;
    d = (dd[0] == ff[3]) ? 4.4 : 5.5;

    int i = (int)d;

    i = i + ii() + (int)x;

    if (i != 12) {
    System.out.println(5);
    System.exit(5);
    }

    System.out.println(0);      
    System.exit(0);      
  }
  static float  f() {
    return (float) 9.5;
  }

  static int ii() {
    return (int)d;
  }
}
