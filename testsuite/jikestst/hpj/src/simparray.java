// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class simparray {
  public static void main(String arg[]) {
    Simpa sa1, sa2;
    sa1 = new Simpa(5,3);
    sa2 = new Simpa(7,4);

    int result = 0;
    result = sa1.sumup();
    result = result + sa2.sumup();

    if (result != 275) {
      System.out.println(99);
      System.exit(99);
      }
    else {
      System.out.println(0);
      System.exit(0);
      }
  }
}
    
    
