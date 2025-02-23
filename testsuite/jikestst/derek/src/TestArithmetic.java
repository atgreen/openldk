// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestArithmetic

class Test
   {
   public static void main(String args[])
      {
      itest();
      ltest();
      ftest();
      dtest();
      }

   static void
   itest()
      {
      int a = 3;
      System.out.print("\nwant: 4\n got: ");          System.out.println(a  +   1);  // iadd
      System.out.print("\nwant: 2\n got: ");          System.out.println(a  -   1);  // isub
      System.out.print("\nwant: 9\n got: ");          System.out.println(a  *   3);  // imul
      System.out.print("\nwant: 1\n got: ");          System.out.println(a  /   2);  // idiv
      System.out.print("\nwant: 1\n got: ");          System.out.println(a  %   2);  // irem
      System.out.print("\nwant: -3\n got: ");         System.out.println(   -   a);  // ineg
      System.out.print("\nwant: 4\n got: ");          System.out.println(   ++  a);  // iinc
      
      a = 0x00000011;
      int b = 0x00000101;

      System.out.print("\nwant: 1\n got: ");          System.out.println(a  &   b);  // iand
      System.out.print("\nwant: 273\n got: ");        System.out.println(a  |   b);  // ior
      System.out.print("\nwant: 272\n got: ");        System.out.println(a  ^   b);  // ixor
      
      a = 0xfffffffd; // -3
      
      System.out.print("\nwant: -6\n got: ");         System.out.println(a  <<  1);  // ishl
      System.out.print("\nwant: -2\n got: ");         System.out.println(a  >>  1);  // ishr
      System.out.print("\nwant: 2147483646\n got: "); System.out.println(a >>>  1);  // iushr
      }
      
   static void
   ltest()
      {
      long a = 10000000000L;
      long b = 2;
                                                              
      System.out.print("\nwant: 10000000002\n got: ");         System.out.println(a +  b);  // ladd
      System.out.print("\nwant: 9999999998\n got: ");          System.out.println(a -  b);  // lsub
      System.out.print("\nwant: 20000000000\n got: ");         System.out.println(a *  b);  // lmul
      System.out.print("\nwant: 5000000000\n got: ");          System.out.println(a /  b);  // ldiv
      System.out.print("\nwant: 0\n got: ");                   System.out.println(a %  b);  // lrem
      System.out.print("\nwant: -2\n got: ");                  System.out.println(  -  b);  // lneg
      System.out.print("\nwant: -10000000000\n got: ");        System.out.println(  -  a);  // lneg

      a = 0x0110000000000011L;
      b = 0x1010000000000101L;

      System.out.print("\nwant: 4503599627370497\n got: ");    System.out.println(a &   b);  // land
      System.out.print("\nwant: 1229482698272145681\n got: "); System.out.println(a |   b);  // lor
      System.out.print("\nwant: 1224979098644775184\n got: "); System.out.println(a ^   b);  // lxor

      a = 0xfffffffffffffffdL; // -3

      System.out.print("\nwant: -6\n got: ");                  System.out.println(a <<  1);  // lshl
      System.out.print("\nwant: -2\n got: ");                  System.out.println(a >>  1);  // lshr
      System.out.print("\nwant: 9223372036854775806\n got: "); System.out.println(a >>> 1);  // lushr
      }

   static void
   ftest()
      {
      float a = 1.0f;
      float b = 2.0f;
                                                              
      System.out.print("\nwant: 3\n got: ");     System.out.println(a + b);  // fadd
      System.out.print("\nwant: -1\n got: ");    System.out.println(a - b);  // fsub
      System.out.print("\nwant: 2\n got: ");     System.out.println(a * b);  // fmul
      System.out.print("\nwant: 0.5\n got: ");   System.out.println(a / b);  // fdiv
      System.out.print("\nwant: -1\n got: ");    System.out.println(  - a);  // fneg

      a = 1.5F; 
      b = 0.9F;
      System.out.print("\nwant: 0.6\n got: ");   System.out.println(a % b);  // frem
      }

   static void
   dtest()
      {
      double a = 1;
      double b = 2;
                                                              
      System.out.print("\nwant: 3\n got: ");     System.out.println(a + b);  // dadd
      System.out.print("\nwant: -1\n got: ");    System.out.println(a - b);  // dsub
      System.out.print("\nwant: 2\n got: ");     System.out.println(a * b);  // dmul
      System.out.print("\nwant: 0.5\n got: ");   System.out.println(a / b);  // ddiv
      System.out.print("\nwant: -1\n got: ");    System.out.println(  - a);  // dneg
   
      a = 1.5;
      b = 0.9;
      System.out.print("\nwant: 0.6\n got: ");   System.out.println(a % b);  // drem
      }
   }
