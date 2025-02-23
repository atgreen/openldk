// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestVirtualCall

class A
   {
   int f() { return 1; }
   }

class B extends A
   {
   int f() { return 2; }
   static int g() { return 3; }
   }
   
class C extends B
   {
   }
   
class Test
   {
   public static void main(String args[])
      {
      A a = new A();
      B b = new B();
      A t = a;

      System.out.print("\nwant: 12\n got: ");
      for (int i = 0; i < 2; ++i)
         {
         int j = t.f();
         System.out.print(j);
         t = b;
         }
      System.out.println();
      
      C c = new C();
      System.out.print("\nwant: 3\n got: ");
      System.out.println(c.g());
      }
   }
