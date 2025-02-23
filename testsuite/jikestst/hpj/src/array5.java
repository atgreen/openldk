// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// Test of ArrayIndexOutOfBounds exception


class Test {
  public static void main (String args[]) {
    int[] a;
    int i,j;
    a = new int[5];
   
    i = 4;
    a[i] = 3;
    j = a[i];

    i = -21767345;
    a[i] = 9;  // i is too small
    System.out.println(5);  // Should NEVER get this far
    System.exit(5);  // Should NEVER get this far
  }
}
