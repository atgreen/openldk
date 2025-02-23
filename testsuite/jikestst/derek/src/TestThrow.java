// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestThrow

import java.io.*;

class MyErrorBase extends Throwable
   {
   }
   
class MyError extends MyErrorBase
   {
   }
   
class NotMyError extends Throwable
   {
   }
   
class Test
   {
   public static void main(String args[])
      throws NotMyError
      {
      
      // test "user" exceptions
      try
         {
         int a = 1;
         int b = 2;
         int c = a + b * foo();
         System.out.println(c);
         }
      catch (MyErrorBase  e)
         {
         System.out.println("caught: " + e);
         }
      
      // test "vm" exceptions
      try
         {
         FileInputStream s = new FileInputStream("xyzzy");
         System.out.println(s);
         }
      catch (IOException e)
         {
         System.out.println("caught: " + e);
         }
      }

   static int foo()
      throws MyError,NotMyError
      {
      if (true ) throw new    MyError();
      else       throw new NotMyError();
      }
   }
