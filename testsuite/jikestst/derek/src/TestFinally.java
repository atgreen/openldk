// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestFinally

class Test
   {
   public static void main(String args[])
      {
      try 
         {
         System.out.println("hi");      // jsr
         return;
         }

      finally 
         {
         System.out.println("bye");
         }                              // ret
      }
   }
