// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class syncm1 {
  int value;
  int other;
  public static void main(String aa[]) throws CNotFound, IOException {
     PrintStream pp;
     int result = 0;
     pp = new PrintStream(new FileOutputStream("syncm1.out"));
     syncm1 s1;
     s1 = new syncm1();
     s1.other = 10;
     pp.println(s1.other);
     try {
     s1.sync1(0);
     }
     catch(Throwable t) {
       result = result + 1;
     }
     pp.println(s1.other);
     try {
     s1.sync1(2);
     }
     catch (Throwable t) {
       result = result + 3;
     }
     pp.println(s1.other);
     s1.sync1(10);
     pp.println(s1.other);      
     System.out.println(result + s1.value);     
     System.exit(result + s1.value);     
  }

  synchronized void sync1(int value) throws CNotFound {
     if (other > 5) {
       other = calc(value);
     }
     other = other + 1;
     if (value == 2) {
        Truck x;
        x = new Truck();
        x.serial = 55;
        throw new CNotFound(x);
     }
  }

  int calc(int v) {
    int result;
    result = 20 / v;
    return result;
  }

}











