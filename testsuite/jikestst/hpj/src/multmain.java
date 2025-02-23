// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class multmain {
  public static void main(String args[]) {
    multchild mc;

    mc = new multchild();
    mc.valuechild = 4;
    mc.valueparent = 5;
    mc.valuegrand = 6;
 
    int x = mc.getvgrand(); 
    int result = 0;

    result = result + x;
    mc.setparent();
    if (mc.valueparent != 5 || mc.getparent() != 99)
      result = result + 10;
    ((multparent)mc).valueparent = 10;
    if (mc.valueparent != 5 || mc.getparent() != 10)
      result = result + 100;

    get((multparent)mc);

    if (mc.valueparent != 5 || mc.getparent() != 50)
      result = result + 1;
    System.out.println(result);
    System.exit(result);

  }

   static void get(multparent mp) {
     mp.valueparent = 50;
   }
}
