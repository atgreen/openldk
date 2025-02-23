// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class truckarray {

   public static void main(String aa[]) {
      Truck ta[];
      ta = new Truck[10];
      int small, medium, large, result;

      setup(ta);
      small = 0; medium = 0; large = 0;
      for (int i=0; i<ta.length; i++) {
        switch (ta[i].maxLoad) {
           case 500  :
           case 1000 :
             small = small + 1;
             break;
           case 1500 :
           case 2000 :
             medium = medium + 1;
             break;
           case 2500 :
           case 3000 :
             large = large + 1;
             break;
           }
        }

      if (small != 3 || medium != 3 || large != 4) 
        result = 5;
      else
        result = 0;

      for (int i=0; i<ta.length; i++) {
        switch (ta[i].numberpassengers) {
           case 1  :
             if (i!= 9) result = result + i;
             break;
           case 2  :
             if (i!= 8) result = result + i;
             break;
           case 3  :
             if (i!= 7) result = result + i;
             break;
           case 4  :
             if (i!= 6) result = result + i;
             break;
           case 5  :
             if (i!= 5) result = result + i;
             break;
           case 6  :
             if (i!= 4) result = result + i;
             break;
           case 7  :
             if (i!= 3) result = result + i;
             break;
           case 8  :
             if (i!= 2) result = result + i;
             break;
           case 9  :
             if (i!= 1) result = result + i;
             break;
           case 10  :
             if (i!= 0) result = result + i;
             break;
           }
        }

      System.out.println(result);
      System.exit(result);
    }


  static void setup(Truck tt[]) {
     int maxloads[] = { 2000, 2500, 3000, 500, 1000, 2000, 1500, 2500, 3000, 500};
     int numpass[] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1 };
     
     for (int i=0; i< tt.length; i++) {
         tt[i] = new Truck();
         tt[i].maxLoad = maxloads[i];
         tt[i].serial = i;
         tt[i].numberpassengers = numpass[i];
       }
  }
}
