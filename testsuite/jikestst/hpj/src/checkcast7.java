// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class checkcast7 {

   public static void main (String aa[]) {
     cost cost1, costarray[];
     foreign farray[], fmd[][];
     Object o;
     String langs_set1[] = {"Thai", "English", "French"};
     String langs_set2[] = {"French", "Indonesian", "Thai"};
     String langs_set3[] = {"English"};
     int result, ok;

     result = 3; ok = 0;

     o = new Bottle(5, "Thailand", langs_set1);
  
     farray = new foreign[3];
     farray[0] = (foreign)o;

     if ( ((cost)farray[0]).price() == 5 &&
         farray[0].hasEnglishLabel())
       ok = ok + 1;

     if (farray[0] instanceof Bottle) ok = ok + 1;

     if (o instanceof foreign) ok = ok + 1;

     farray[1] = new Bottle(2, "Indonesia", langs_set2);
     farray[2] = new Bottle(9, "USA", langs_set3);

     fmd = new foreign[1][];
     fmd[0] = farray;

     ((Bottle)fmd[0][1]).costofitem = ((cost)fmd[0][2]).price();

     cost1 = new Bottle(((Bottle)fmd[0][1]).price(), "Canada", langs_set3);
     costarray = new cost[2];
     costarray[0] = cost1;
     costarray[1] = (cost)farray[2];
     System.arraycopy(costarray, 0, farray, 0, 2);

     for (int i = 0; i< fmd[0].length; i++) {
       result = result + ((cost)fmd[0][i]).price();
     }

     for (int i = 0; i< costarray.length; i++) {
       if ( ((foreign)costarray[i]).hasEnglishLabel()) 
        result = result + 1;
     }

     if (ok != 3) result = result + 5;

     System.out.println(result);
     System.exit(result);

   }
}
