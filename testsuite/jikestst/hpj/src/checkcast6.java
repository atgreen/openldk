// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class checkcast6 {

   public static void main (String aa[]) {
     cost costarray[], costcans[];
     int result, ok;
     costarray = new cost[5];
     costcans = new cost[5];
     for (int i=0; i< 5; i++) {
       costarray[i] = new Food(i);
       costcans[i] = new Cans(i, i+2);
     }
     costarray[0] = new Cans(15, 3);

     result = 3; ok = 0;
     Cans canarray[];
     try {
       canarray = (Cans[])costcans;
       result = result + canarray[1].price();
     }
     catch (ClassCastException e) {
      ok = ok + 1;
     }

     canarray = new Cans[5];
     System.arraycopy(costcans, 0, canarray, 0, 5);
     if (canarray[2].price() == 2) ok = ok + 1;

     try {
      canarray = (Cans[])costarray;
      result = result + canarray[1].price();
     }
     catch (ClassCastException e) {
      ok = ok + 1;
     }

     try {
      System.arraycopy(costarray, 0, canarray, 0, 5);
      result = result + canarray[1].price();
     }
     catch (ArrayStoreException e) {
      if (canarray[0].price() == 15) 
        ok = ok + 1;
     }

     if (ok != 4) result = result + 5;
     System.out.println(result);
     System.exit(result);
   }
}
