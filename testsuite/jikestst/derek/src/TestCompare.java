// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestCompare

class Test
   {
   public static void main(String args[])
      {
      zero_cmp();
      i_cmp();
      l_cmp();
      f_cmp();
      d_cmp();
      a_cmp();
      null_cmp();
      }

   static void
   zero_cmp()
      {
      int i = -1;

      System.out.print("\nwant: 100110\n got: ");
      if (i != 0) System.out.print(1); else System.out.print(0); // ifeq
      if (i == 0) System.out.print(1); else System.out.print(0); // ifne
      if (i >= 0) System.out.print(1); else System.out.print(0); // iflt
      if (i <  0) System.out.print(1); else System.out.print(0); // ifge
      if (i <= 0) System.out.print(1); else System.out.print(0); // ifgt
      if (i >  0) System.out.print(1); else System.out.print(0); // ifle
      System.out.println();
      }

   static void
   i_cmp()
      {
      int i = -1;
      int j =  0;

      System.out.print("\nwant: 100110\n got: ");
      if (i != j) System.out.print(1); else System.out.print(0); // if_icmpeq
      if (i == j) System.out.print(1); else System.out.print(0); // if_icmpne
      if (i >= j) System.out.print(1); else System.out.print(0); // if_icmplt
      if (i <  j) System.out.print(1); else System.out.print(0); // if_icmpge
      if (i <= j) System.out.print(1); else System.out.print(0); // if_icmpgt
      if (i >  j) System.out.print(1); else System.out.print(0); // if_icmple
      System.out.println();
      }

   static void
   l_cmp()
      {
      long a = 1;
      long b = 2;
      
      System.out.print("\nwant: 100010001\n got: ");
      
      if (a <  b) System.out.print(1); else System.out.print(0); // lcmp(-1)
      if (a == b) System.out.print(1); else System.out.print(0);
      if (a >  b) System.out.print(1); else System.out.print(0);
      
      if (a <  a) System.out.print(1); else System.out.print(0);
      if (a == a) System.out.print(1); else System.out.print(0); // lcmp(0)
      if (a >  a) System.out.print(1); else System.out.print(0);
      
      if (b <  a) System.out.print(1); else System.out.print(0);
      if (b == a) System.out.print(1); else System.out.print(0);
      if (b >  a) System.out.print(1); else System.out.print(0); // lcmp(1)
      
      System.out.println();
      }
      
   static void
   f_cmp()
      {
      float a = 1;
      float b = 2;
      
      System.out.print("\nwant: 100010001\n got: ");
      
      if (a <  b) System.out.print(1); else System.out.print(0); // fcmp[lg](-1)
      if (a == b) System.out.print(1); else System.out.print(0);
      if (a >  b) System.out.print(1); else System.out.print(0);
      
      if (a <  a) System.out.print(1); else System.out.print(0);
      if (a == a) System.out.print(1); else System.out.print(0); // fcmp[lg](0)
      if (a >  a) System.out.print(1); else System.out.print(0);
      
      if (b <  a) System.out.print(1); else System.out.print(0);
      if (b == a) System.out.print(1); else System.out.print(0);
      if (b >  a) System.out.print(1); else System.out.print(0); // fcmp[lg](1)
      
      System.out.println();
      }
      
   static void
   d_cmp()
      {
      double a = 1;
      double b = 2;
      
      System.out.print("\nwant: 100010001\n got: ");
      
      if (a <  b) System.out.print(1); else System.out.print(0); // dcmp[lg](-1)
      if (a == b) System.out.print(1); else System.out.print(0);
      if (a >  b) System.out.print(1); else System.out.print(0);
      
      if (a <  a) System.out.print(1); else System.out.print(0);
      if (a == a) System.out.print(1); else System.out.print(0); // dcmp[lg](0)
      if (a >  a) System.out.print(1); else System.out.print(0);
      
      if (b <  a) System.out.print(1); else System.out.print(0);
      if (b == a) System.out.print(1); else System.out.print(0);
      if (b >  a) System.out.print(1); else System.out.print(0); // dcmp[lg](1)
      
      System.out.println();
      }
      
   static void
   a_cmp()
      {
      Object a = null;
      Object b = null;
      System.out.print("\nwant: 10\n got: ");
      if (a == b) System.out.print(1); else System.out.print(0); // if_acmpne
      if (a != b) System.out.print(1); else System.out.print(0); // if_acmpeq
      System.out.println();
      }

   static void
   null_cmp()
      {
      Object o = null;
      System.out.print("\nwant: 10\n got: ");
      if (o == null) System.out.print(1); else System.out.print(0); // ifnonnull
      if (o != null) System.out.print(1); else System.out.print(0); // ifnull
      System.out.println();
      }
   }
