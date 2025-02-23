// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// MATRIX Multiply is associative
class callmm {
  public static void main(String aa[]) {
     double a[][], b[][], c[][], ab[][], bc[][], abc1[][], abc2[][], diff;
     int result;

     a = new double[4][5];
     b = new double[5][7];
     c = new double[7][6];
     ab = new double[4][7];
     bc = new double[5][6];
     abc1 = new double[4][6];
     abc2 = new double[4][6];

     a[1][1] = 3.0;
     a[1][2] = 2.0;
     a[1][3] = 4.0;
     a[1][4] = 10.0;
     a[2][1] = 4.0;
     a[2][2] = 2.0;
     a[2][3] = -9.0;
     a[2][4] = 11.0;
     a[3][1] = 5.0;
     a[3][2] = -3.0;
     a[3][3] = -1.0;
     a[3][4] = 10.0;

     b[1][1] = 5.0;
     b[1][2] = 11.0;
     b[1][3] = 4.0;
     b[1][4] = -1.0;    
     b[1][5] = 8.0;    
     b[1][6] = 4.0;    
     b[2][1] = 4.0;
     b[2][2] = 10.0;
     b[2][3] = 6.0;
     b[2][4] = 1.0;    
     b[2][5] = 4.0;    
     b[2][6] = -9.0;    
     b[3][1] = 3.0;
     b[3][2] = 2.0;
     b[3][3] = 5.0;
     b[3][4] = 11.0;    
     b[3][5] = -5.0;    
     b[3][6] = -9.0;    
     b[4][1] = 12.0;
     b[4][2] = -3.0;
     b[4][3] = 4.0;
     b[4][4] = 1.0;   
     b[4][5] = 7.0;   
     b[4][6] = 4.0;          

     c[1][1] = 5.0;
     c[1][2] = 1.0;
     c[1][3] = 4.0;
     c[1][4] = 5.0;   
     c[1][5] = -4.0;   
     c[2][1] = 3.0;
     c[2][2] = 1.0;
     c[2][3] = 8.0;
     c[2][4] = 4.0;   
     c[2][5] = 18.0;   
     c[3][1] = 12.0;
     c[3][2] = 8.0;
     c[3][3] = -4.0;
     c[3][4] = -5.0;   
     c[3][5] = 22.0;   
     c[4][1] = 11.0;
     c[4][2] = 4.0;
     c[4][3] = 2.0;
     c[4][4] = 7.0;   
     c[4][5] = 3.0;   
     c[5][1] = 10.0;
     c[5][2] = 1.0;
     c[5][3] = -3.0;
     c[5][4] = 4.0;  
     c[5][5] = 11.0;   
     c[6][1] = -1.0;
     c[6][2] = -1.0;
     c[6][3] = 4.0;
     c[6][4] = -9.0;   
     c[6][5] = 8.0;   

     result = 5;


     mmult.mmult1(a,b,ab,3,4,6);
     mmult.mmult2(ab,c,abc1,3,6,5);

     mmult.mmult1(b,c,bc,4,6,5);
     mmult.mmult2(a,bc,abc2,3,4,5);

     diff = 0.0;
     for (int i=1; i<=3; i++) {
       for (int j=1; j<=5; j++) {
          diff = diff + abc1[i][j] - abc2[i][j];
        }
     }
     if (diff < 0.000000001) result = 10;

     System.out.println(result);
     System.exit(result);
  }
}
