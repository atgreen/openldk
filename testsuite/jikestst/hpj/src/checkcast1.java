// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class checkcast1 {

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

      ((Truck[])xx)[1].maxLoad = ((Truck[])xx)[1].maxLoad + 10;

      ((Truck[])xx)[2].maxLoad = 3;

      xx = ta[2];
      ((Pickup)xx).iscamper = true;

      xx = ta[3];
      if ((Pickup)xx != null) {
      System.out.println(15);
      System.exit(15);
      }

      xx = ta;

      if (((Object[])xx).length != 5) {
      System.out.println(5);
      System.exit(5);
      }
 
      if (ta[1].maxLoad != 15 || ta[2].maxLoad != 3 || ta[2].numberpassengers != 6)
      {
        System.out.println(13);
        System.exit(13);
	}
 
      if (!((Pickup)ta[2]).iscamper) {
      System.out.println(21);
      System.exit(21);
      }

      System.out.println(1);
      System.exit(1);

     }
}
