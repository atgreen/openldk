// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestClassInitializer

class A
   {
   static         { System.out.println("clinit called for A"); }
   static int f() { System.out.println("A.f called"); return 123; }
   static int i = f();
   }

class B
   {
   B()        { }
   static     { System.out.println("clinit called for B"); }
   int    f() { System.out.println("B.f called"); return 456; }
   }
   
class Test
   {
   public static void main(String args[])
      {
      int i = A.i;     System.out.println(i);     // test initialization before first field reference
      B   b = new B(); System.out.println(b.f()); // test initialization before first instance creation
      }
   }
