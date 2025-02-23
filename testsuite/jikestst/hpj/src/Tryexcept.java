// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Tryexcept {
  public static void main (String args[]) {
    int[] a;
    int i, result, b[] = {0,1,2,3,4};
    Object obj, obja[];
    Cloneable clone[];
   
    result = 10;
    a = null; obja = null;
    for (i=0; i < 8; i++) {
      try {
        switch (i) {
	case 0 : {  // Null Pointer E.
          a[2] = 3;
          result = result + a[2];
	}
        case 1 : { // Class Cast E.
          a = b;
          a[1] = 2;
          obj = a;
          clone = (Cloneable[])obj;
          result = result + ((int[])(clone[0]))[1];
	}
	case 2: { // Arith trap E.
          result = result + i/(a[1]-2);
	}
        case 3: { // Array Store
          obja = new String[3];
          obja[0] = "abe";
          obja[1] = new Integer(4);
          result = result + obja.length;
        }
        case 4: { // Negative Array Size E.
          obja = new int[a[1]-5][];
          result = result + obja.length;
        }
        case 5: { // Num Format E.
          obj = "123";
          a[1] = Integer.parseInt((String)obj);
          obj = "abc";
          a[2] = Integer.parseInt((String)obj);
          result = result + a[1] + a[2];
	}
	case 6: { // Clone Not Supported E.
          Tryexcept ee, ef;
          ee = new Tryesub(4);
          ef = (Tryexcept)ee.clone();
          if (((Tryesub)ef).i == 4) {
            ee = new Tryexcept();
            ef = (Tryexcept)ee.clone();
          }
          result = result + ((Tryesub)ef).i;
        }
        case 7: { // ArrayBounds E.
          obja[10] = "12";
          result = result + Integer.parseInt((String)obja[1]);
        }
      }
      }
      catch(NullPointerException e) {
         if (i != 0) {
           result = result + 1;
           e.printStackTrace();
	 }
         System.out.println(result);
      }
      catch(ClassCastException e) {
         if (i != 1) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(ArithmeticException e) {
         if (i != 2) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(ArrayStoreException e) {
         if (i != 3) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(NegativeArraySizeException e) {
         if (i != 4) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(NumberFormatException e) {
         if (i != 5) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(CloneNotSupportedException e) {
         if (i != 6) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      catch(ArrayIndexOutOfBoundsException e) {
         if (i != 7) {
           result = result + 1;
           e.printStackTrace();
	 }
      }
      System.out.println("result = " + result);
      System.out.println("i = " + i);
    }
    if (i != 8) result = result + 10;
    System.out.println(result);
    System.exit(result);
  }
}
