// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class ctestinit {
  public static void main(String aa[]) {
     cinit cc;
     int x, y, z;

     Fruit f;
     f = new Fruit();   // Fruit's count becomes 1.

     x = cinit.i1;      // i1 is based on Fruit's count
     y = cinit.i2;      // i2 only correct if  Basket is inited

     cc = new cinit();
     z = cc.i3;

     if (cc.t1.seeds != 3){
       System.out.println(1);
       System.exit(1);
       }
     if (x != 7){
       System.out.println(2);
       System.exit(2);
       }
     if (y != 10) { 
       System.out.println(3);
       System.exit(3);
       }

     if (z != 16) {
       System.out.println(5);
       System.exit(5);
       }

     System.out.println(10);
     System.exit(10);
  }
}
