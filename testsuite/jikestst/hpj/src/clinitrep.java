// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class clinitrep {
  static int y = initrep.z;
  static int x = 5;
  public static void main(String xx[]) {
    if (x != 5) {
    System.out.println(5);
    System.exit(5);
    }
    if (y != 5) {
    System.out.println(10);
    System.exit(10);
    }
  }
 }


