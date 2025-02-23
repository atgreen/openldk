// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


class Test {
   public static void main(String args[]) {
      int i;
      int result;
      long j;
      result = 0;

      i = -4;
      i = i << 3;
      
      if (i != -32) result = result + 1;

      i = i + 5;
      i = i & 0xF;
      if (i != 5) result = result + 10;

      j = 250;
      j = j << 33;

      if (j != 2147483648000L) result = result + 100;

      if (result !=0) {
      System.out.println(1);
      System.exit(1);
      }
      System.out.println(0);
      System.exit(0);
  }
}
