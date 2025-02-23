// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// %GROUP Cans Food cost order 
// %TSR EE 23
// %MAIN

package tst;

class instance {

   public static void main (String aa[]) {
      Food ff, f2[][];
      cost cxy[], cz[], cq[];
      order oxy[], oz[], oq[];
      Cans can1, canarray[];
      Cloneable clone1, clonea[];
      Object o1, oa[];
      int result, sum;

      result = 0; sum = 0;

      ff = new Food(3);
      can1 = new Cans(10,4);
      canarray = new Cans[3];
      canarray[0] = new Cans(2,9);
      canarray[1] = new Cans(6,19);
      canarray[2] = new Cans(1, 8);

      cxy = new cost[3];
      oxy = new order[3];

      cxy[0] = new Food(1);
      cxy[1] = ff;
      cxy[2] = new Cans(5,2);

      o1 = cxy[0];
      oa = cxy;

      f2 = new Food[5][];
      f2[0] = new Food[2];
      f2[0][0] = (Food)cxy[0];
      f2[1] = new Food[2];
      f2[1][0] = (Food)cxy[1];

      if (o1 instanceof Food) result = result + 1;
      if (cxy instanceof Object[]) result = result + 1;
      if (oxy instanceof Cloneable) result = result + 1;
      if (!(oa instanceof Food[])) result = result + 1;
      if (oa instanceof cost[]) result = result + 1;
      if (oa[0] instanceof Food) result = result + 1;

      clone1 = cxy;
      clonea = new Cloneable[3];
      clonea[0] = cxy;

      if (clone1 instanceof Object[]) result = result + 1;
      if (clonea instanceof Object)  result = result + 1;
      if (clonea instanceof Object[]) result = result +1;
      if (clonea[0] instanceof Object[]) result = result + 1;
      if (clonea[0] instanceof cost[]) result = result + 1;

      if (f2 instanceof Object[]) result = result + 1;
      if (f2[1] instanceof Object[]) result = result + 1;
      if (f2 instanceof Food[][]) result = result + 1;
      if (f2[1][0] instanceof cost) result = result + 1;
      if (f2 instanceof cost[][]) result = result + 1;
      if (cxy instanceof cost[]) result = result + 1;
      if (cxy[2] instanceof order) result = result + 1;
      if (cxy[2] instanceof Cans) result = result + 1;

      for (int i=0; i<3; i++) {
       sum = sum + cxy[1].price();
      }
      sum = sum + (((cost[])clone1)[0].price());
      if (((cost[])clone1)[0].price() == ((cost[])clonea[0])[0].price())
        result = result + 1;

      f2 = new Food[1][2];
      f2[0][0] = new Food(5);
      f2[0][1] = new Food(3);
      clonea[1] = f2;
      if (((cost[][])clonea[1])[0][0].price() == cxy[2].price())
        result = result + 1;
      oxy[0] = new Cans(4,3);
      oxy[1] = can1;
      oxy[2] = new Cans(15,2);
      for (int i=0; i<3; i++) {
        sum = sum + oxy[1].price();
      }

      cxy[0] = oxy[0];
      cxy[1] = can1;
      oxy[1] = (order)cxy[1];
      cxy[2] = oxy[1];
      for (int i=0; i<3; i++) {
        sum = sum + cxy[i].price();
      }

      if (oxy instanceof cost[] &&
          oxy[0] instanceof Food &&
          oxy[1] instanceof Cans) result = result + 1;
      if (canarray instanceof cost[]) result = result + 1;
      if (canarray instanceof Object[]) result = result + 1;

      if (sum == 72) result = result + 1;
      System.out.println(result);
      System.exit(result);
     }
}
