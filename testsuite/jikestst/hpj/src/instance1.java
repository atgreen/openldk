// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class instance1 {

   public static void main (String aa[]) {
      Truck ta[];
      int ii[];
      int result = 0;
      Object xx;

      ta = new Truck[5];
      ta[1] = new Truck();
      ta[2] = new Pickup();
      ii = new int[3];

      if (ta instanceof Truck[]) 
        result = result + 0;
      else
        result = result + 1;
      if (ta instanceof Vehicle[])
        result = result * 1;
      else
        result = result + 10;

      if (ta[1] instanceof Truck)
        result = result + 0;
      else
        result = result + 5;

      if (ta[2] instanceof Truck)
        result = result * 1;
      else 
        result = result + 2;

//  Next gets javac error as it is impossible to be true
//      if (ta instanceof Truck)
//        System.out.println("ta instanceof Truck");  

      if (ta[1] instanceof Pickup)
        result = result + 100;
      else
        result = result + 0;
      if (ta[2] instanceof Pickup)
        result = result + 0;
      else
        result = result + 20;

      if (ta[3] instanceof Truck)
        result = result + 50;
      else
        result = result + 0;
      if (ta[2] instanceof Object)
        result = result + 0;
      else
        result = result + 60;

      if (ta instanceof Object)
        result = result * 1;
      else
        result = result + 1000;
      if (ta instanceof Object[])
        result = result * 1;
      else 
        result = result + 300;

      if (ii instanceof int[])
        result = result * 1;
      else
        result = result + 700;
      if (ii instanceof Object)
        result = result + 0;
      else
        result = result + 10000;

      xx = ta;
      if (xx instanceof Truck[])
        result = result + 0;
      else
        result = result + 3000;
      if (xx instanceof Object)
        result = result + 0;
      else
        result = result + 5000;

      xx = ii;
      if (xx instanceof Truck[])
         result = result + 9000;
      else
         result = result * 1;
      if (xx instanceof int[])
         result = result * 1;
      else
         result = result + 30000;
      if (xx instanceof Object)
         result = result + 0;
      else
         result = result + 50000;

      System.out.println(result);
      System.exit(result);

// Not allowed
//      if (ii instanceof Object[])
//        System.out.println("ii instanceof Object[]");
     }
}
