// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestFieldAccess

class Base
   {
   static boolean s0 = true;
   static byte    s1 = -2;
   static char    s2 = 0xff41;     // 'A'
   static short   s3 = -2;
   static int     s4 = -2;
   static long    s5 = -2;
   static float   s6 = -2;
   static double  s7 = -2;
   static Object  s8 = new Base();
   
          boolean x0 = true;
          byte    x1 = -2;
          char    x2 = 0xff41;     // 'A'
          short   x3 = -2;
          int     x4 = -2;
          long    x5 = -2;
          float   x6 = -2;
          double  x7 = -2;
          Object  x8 = this;
   
   public String toString() { return "Instance of " + getClass().getName(); }
   }

class Test
   {
   public static void main(String args[])
      {
      Base b = new Base();

      System.out.print("\nwant: true\n got: ");             System.out.println(b.s0);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s1);
      System.out.print("\nwant: A\n got: ");                System.out.println(b.s2);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s3);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s4);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s5);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s6);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.s7);
      System.out.print("\nwant: Instance of Base\n got: "); System.out.println(b.s8);

      System.out.print("\nwant: true\n got: ");             System.out.println(b.x0);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x1);
      System.out.print("\nwant: A\n got: ");                System.out.println(b.x2);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x3);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x4);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x5);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x6);
      System.out.print("\nwant: -2\n got: ");               System.out.println(b.x7);
      System.out.print("\nwant: Instance of Base\n got: "); System.out.println(b.x8);
      }
   }
