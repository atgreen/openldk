// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestInterfaceCall

interface InterfaceFoo
   {
   int one = 1;
   int foo();
   }

interface InterfaceBar
   {
   int two = 2;
   int bar();
   }

class A implements InterfaceFoo, InterfaceBar
   {
   public int foo() { return 1; }
   public int bar() { return 2; }
   }
   
class B implements InterfaceBar, InterfaceFoo
   {
   public int bar() { return 3; }
   public int foo() { return 4; }
   }

class C extends B implements InterfaceFoo
   {
   }
   
class Test
   {
   public static void main(String args[])
      {
      InterfaceFoo foo = null;
      foo = new A(); System.out.println(foo.foo());   // 1
      foo = new B(); System.out.println(foo.foo());   // 4
      
      InterfaceBar bar = null;
      bar = new A(); System.out.println(bar.bar());   // 2
      bar = new B(); System.out.println(bar.bar());   // 3
      
      foo = new C(); System.out.println(foo.foo());   // 4
      }
   }
