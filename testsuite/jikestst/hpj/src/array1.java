// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class array1 {
  public static void main (String args[]) {
    Vehicle fleet[];
//    Vehicle shipment[][];
    int[] a;
    int i;
    a = new int[5];
   
    a[1] = 3;
    i = a[1];

    fleet = new Truck[4];
//    shipment = new Truck[3][];
//    fleet[1] = new Vehicle();
    System.out.println(i);
    System.exit(i);
  }
}
