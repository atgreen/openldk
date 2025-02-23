// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class array4 {
  public static void main (String args[]) {
    Vehicle fleet[];
    Vehicle shipment[][];
    Object xx;
    int[][] a;
    int i,sum;
    a = new int[5][];
   
    a[1] = new int[3];
    a[1][2] = 5;
    a[1][1] = 6;
    a[1][0] = 7;
    sum = 0;
    for (i = 0; i<a[1].length; i++)
     sum = sum + a[1][i];
    if (sum != 18) {
    System.out.println(18);
    System.exit(18);
    }

    fleet = new Truck[4];
    fleet[1] = new Truck();
    xx = new Truck[3];
    fleet[1].numberpassengers = 2;
    shipment = new Truck[3][2];
    shipment[1][1] = new Truck();
    shipment[1][1].numberpassengers = 5;
    shipment[2][1] = new Truck();
    shipment[2][1].numberpassengers = 3;
    i = shipment[1][1].numberpassengers - shipment[2][1].numberpassengers;
    System.out.println(i);
    System.exit(i);
  }
}
