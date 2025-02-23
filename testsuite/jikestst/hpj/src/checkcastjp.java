// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class checkcastjp {

   public static void main (String aa[]) throws Exception {
      Truck ta[];
      int ii[];
      int result = 0;
      Object xx;
      Vehicle vv[];

      ta = new Truck[5];
      ta[1] = new Truck();
      ta[1].numberpassengers = 3;
      ta[2] = new Pickup();
      ii = new int[3];

      vv = ta;
      ((Truck[])vv)[1].maxLoad = 5;

      xx = ta;
      ((Vehicle[])xx)[2].numberpassengers = 6;

      for (int i=0; i < 2; i++) {
      try {
        if (ta[1].maxLoad == 5) {
          ta[1].maxLoad = 6;
          // Faking a ClassCastException
          ccthrow();
        }
	// About to cause the real ClassCastException
      ((Pickup[])xx)[1].maxLoad = ((Pickup[])xx)[1].maxLoad + 10;
      }
      catch (ClassCastException e) {
        ta[1].numberpassengers = ta[1].numberpassengers + 1; 
      }

      }

      if (ta[1].maxLoad != 6 || ta[1].numberpassengers != 5 || 
          ta[2].numberpassengers != 6)   {
        System.out.println(13);
        System.exit(13);
	}
 
      System.out.println(15);
      System.exit(15);

     }

      static void ccthrow() throws ClassCastException {
          throw new ClassCastException("stuff");
      }
}
