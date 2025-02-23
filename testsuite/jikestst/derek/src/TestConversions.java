// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestConversions

class Test
   {
   public static void main(String args[])
      {
      i2b();
      i2c();
      i2s();
      i2l();
      l2i();

      i2f();
      i2d();

      l2f();
      l2d();
      
      f2d();
      f2l();
      f2i();
      
      d2i();
      d2l();
      d2f();
      }
      
   static void
   i2b()
      {
      byte x; int  i;
      i = 0x0000007f; x = (byte)i; i = x; System.out.print("\nwant: 127\n got: "); System.out.println(i);
      i = 0x000000ff; x = (byte)i; i = x; System.out.print("\nwant: -1\n got: ");  System.out.println(i);
      i = 0xffffffff; x = (byte)i; i = x; System.out.print("\nwant: -1\n got: ");  System.out.println(i);
      }
      
   static void
   i2c()
      {
      char x; int  i;
      i = 0x0000007f; x = (char)i; i = x; System.out.print("\nwant: 127\n got: ");   System.out.println(i);
      i = 0x000000ff; x = (char)i; i = x; System.out.print("\nwant: 255\n got: ");   System.out.println(i);
      i = 0xffffffff; x = (char)i; i = x; System.out.print("\nwant: 65535\n got: "); System.out.println(i);
      }
      
   static void
   i2s()
      {
      short x; int  i;
      i = 0x00007fff; x = (short)i; i = x; System.out.print("\nwant: 32767\n got: "); System.out.println(i);
      i = 0x0000ffff; x = (short)i; i = x; System.out.print("\nwant: -1\n got: ");    System.out.println(i);
      }

   static void
   i2l()
      {
      long x; int  i;
      i = 0x7fffffff; x = (long)i; System.out.print("\nwant: 2147483647\n got: "); System.out.println(x);
      i = 0xffffffff; x = (long)i; System.out.print("\nwant: -1\n got: ");         System.out.println(x);
      }

   static void
   l2i()
      {
      long x; int  i;
      x = 0x000000007fffffffL; i = (int)x; System.out.print("\nwant: 2147483647\n got: "); System.out.println(i);
      x = 0x00000000ffffffffL; i = (int)x; System.out.print("\nwant: -1\n got: ");         System.out.println(i);
      }

   static void
   i2f()
      {
      int   i = -2;
      float f = i;
      System.out.print("\nwant: -2\n got: "); System.out.println(f);
      }

   static void
   l2f()
      {
      long  l = -2;
      float f = l;
      System.out.print("\nwant: -2\n got: "); System.out.println(f);
      }

   static void
   l2d()
      {
      long   l = -2;
      double d = l;
      System.out.print("\nwant: -2\n got: "); System.out.println(d);
      }

   static void
   i2d()
      {
      int    i = -2;
      double d = i;
      System.out.print("\nwant: -2\n got: "); System.out.println(d);
      }

   static void
   f2d()
      {
      float  f = -2.0f;
      double d = f;
      System.out.print("\nwant: -2\n got: "); System.out.println(d);
      }

   static void
   f2l()
      {
      float  f = -2.0f;
      long   l = (long)f;
      System.out.print("\nwant: -2\n got: "); System.out.println(l);
      }

   static void
   f2i()
      {
      float  f = -2.0f;
      int    i = (int)f;
      System.out.print("\nwant: -2\n got: "); System.out.println(i);
      }
   
   static void
   d2i()
      {
      double d = -2;
      int    i = (int)d;
      System.out.print("\nwant: -2\n got: "); System.out.println(i);
      }

   static void
   d2l()
      {
      double d = -2;
      long   l = (long)d;
      System.out.print("\nwant: -2\n got: "); System.out.println(l);
      }

   static void
   d2f()
      {
      double d = -2;
      float  f = (float)d;
      System.out.print("\nwant: -2\n got: "); System.out.println(f);
      }
   }
