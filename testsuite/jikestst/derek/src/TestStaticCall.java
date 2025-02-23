// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestStaticCall

class Test
   {
   public static void main(String args[])
      {
      int i = f(111);
      System.out.print("\nwant: 333\n got: "); System.out.println(i);
      }
      
   static int
   f(int arg)
      {
      return arg + 222;
      }
   }
