// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
  public static void main(String args[]) {
    int i = 6;
    int x = 1;
    for (int j=0; j<2; j++) {
     try {
       i = i + 1;
       i = other(i);
     }
     catch (ArrayIndexOutOfBoundsException e) {
       i = i + 1;
     }
     catch (Exception e) {
        x = x + 10;
        i = i + 1;
     }
    }
    System.out.println(i+x);
    System.exit(i+x);
  }

  static int other(int i) throws Exception {
    int a[];
    try {
    if (i == 7) {
      i = otherother(i);
    }
    }
    catch (java.io.IOException e) {
      i = 2;
    }
    return i;
  }
  
  static int otherother(int i) throws Exception {
     int a[];
     a = new int[4];
     try {
       a[i]  = 5;   // throws an Array out of bounds exception 
                    // select code should jump back up to main.
       if (i==7) throw new java.io.IOException("wrong value of i");
     }
     catch (java.io.IOException e) {
        System.out.println("in catch of otherother");
     }
     return i-2;
  }
}
