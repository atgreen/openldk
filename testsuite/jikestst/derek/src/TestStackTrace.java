// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestStackTrace

import java.io.*;
   
class Test
   {
   Test()
      throws Exception
      {
      try{
         testSoftwareException();
         }
      catch (Exception e)
         {
         throw e;
         }
      }
   
   static void
   testHardwareException()
      throws Exception
      {
      int i = 1; int j = 0; int k = i / j;
      System.out.println(k);
      }
   
   static void
   testSoftwareException()
      throws Exception
      {
      Float f = Float.valueOf("abc");
      System.out.println(f);
      }
   
   static void
   testUserException()
      throws Exception
      {
      throw new IOException();
      }
      
   static void
   testRethrownException()
      throws Exception
      {
      new Test();
      }
   
   static void
   trouble(int choice)
      throws Exception
      {
      if (choice == 1) testHardwareException();
      if (choice == 2) testSoftwareException();
      if (choice == 3) testUserException();
      if (choice == 4) testRethrownException();
      }
      
   public static void main(String args[])
      throws Exception
      {
      for (int i = 1; i <= 4; ++i)
         {
         System.out.println("test " + i);
         try{
            trouble(i);
            }
         catch (Exception e)
            {
            System.out.println("caught " + e);
            e.printStackTrace(System.out);
            }
         }
      }
   }
