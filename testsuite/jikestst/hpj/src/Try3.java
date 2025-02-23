// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
  public static void main(String args[]) {
    int i, j;
    j = 0;
    for (i = 0; i < 5; i++) {
      try {
        j = j + 1;
      } finally {
        j = j * 2;
        if (i > 1) {
          break;
        }
      }
    }
    System.out.println(j);
    System.exit(j);
  }
}
