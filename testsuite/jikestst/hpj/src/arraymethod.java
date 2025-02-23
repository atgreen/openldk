// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


class Test {
  static double[] f(double[] xx) {
    double fx[] = new double[xx.length];
    for (int i = 0; i< xx.length; i++)
      fx[i] = 3.4 + i;
    return fx;
  }

  public static void main(String aa[]) {
    double[] x;
    x = new double[4];
    int i = 1,j;
    double[] y;
    double sum = 0.0;
    
    
    y = f(x);
    for (j=0; j<y.length; j++) {
      System.out.println(y[j]);
      sum = sum + y[j];
    }
    sum = sum + f(x)[i];

    System.out.println(sum);
    if (sum != 24.0) {
      System.out.println(5);
      System.exit(5);
      }
    else {
      System.out.println(0);
      System.exit(0);
      }
  }
}


  
    
