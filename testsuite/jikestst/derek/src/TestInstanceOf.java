// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestInstanceOf

class Test
   {
   public static void main(String args[])
      {
      Object o1     = new Test();         // source: a reference
      Object o2[]   = new Test[2];        // source: an array of references
  //  Object o3[][] = new Object[2][];    // source: an array of arrays       // !!TODO "multianewarray" not ready yet
  //         o3[0]  = new Test[4];
  //         o3[1]  = new Test[4];
      int    o4[]   = new int [2];        // source: an array of primitives

      System.out.print("\nwant: true false false false\n got: ");  test(o1);
      System.out.print("\nwant: false true false false\n got: ");  test(o2);
  //  System.out.print("\nwant: false false false false\n got: "); test(o3);  // !!TODO "multianewarray" not ready yet
      System.out.print("\nwant: false false false true\n got: ");  test(o4);

      o1 = (Test)o1;   //  ok
   // o1 = (String)o1; //  throw exception
      }

   static void test(Object o)
      {
      boolean b1 = o instanceof Test    ;  // target: a reference
      boolean b2 = o instanceof Test[]  ;  // target: an array of references
      boolean b3 = o instanceof Test[][];  // target: an array of arrays
      boolean b4 = o instanceof int []  ;  // target: an array of primitives

      System.out.println(b1 + " " + b2 + " " + b3 + " " + b4);
      }
   }
