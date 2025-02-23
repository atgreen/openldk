// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Simpa {
  int array1[];
  byte array2[];
  int start = 5;
  
  Simpa(int dim, int modifier) {
    array1 = new int[dim];
    array2 = new byte[dim+3];

    for (int i = 0; i<dim; i++) {
      array1[i] = start - modifier + i;
     }

    for (int i = 0; i<array2.length; i++) {
      array2[i] = (byte)(start + modifier + i);
     }

  }

  int sumup() {
    int sum = 0;
    for (int i = 0; i < array1.length; i++) {
      sum = sum + array1[i];
     }

    for (int i = 0; i< array2.length; i++) {
      sum = sum + array2[i];
     }

    return sum;
  }
}
