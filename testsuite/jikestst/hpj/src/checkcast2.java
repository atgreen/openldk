// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class checkcast2 {

   public static void main (String aa[]) {
      Truck ta[];
      int ii[];
      int result = 0;
      Object xx;
      Vehicle vv[];

      ta = new Truck[5];
      ta[1] = new Truck();
      ta[2] = new Pickup();
      ii = new int[3];

      vv = ta;
      ((Truck[])vv)[1].maxLoad = 5;

      xx = ta;
      ((Vehicle[])xx)[2].numberpassengers = 6;

      ((Pickup[])xx)[1].maxLoad = ((Pickup[])xx)[1].maxLoad + 10;

      if (ta[1].maxLoad != 15) { // should NEVER get this far. Last stmt should trap
        System.out.println(13);
        System.exit(13);
	}
 
      System.out.println(11);  // should NEVER get this far either.
      System.exit(11);  // should NEVER get this far either.

     }
}
