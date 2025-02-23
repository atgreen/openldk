// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;
import java.util.Date;
   
class Test
   {
   public static void main(String args[])
      throws Exception
      {
      foo();
      }
   
   public static void foo()
      throws Exception
      {
      
/**
      Date date = new Date(0,0,0);
      System.out.println(date);
**/

      String filename = null;//"foo";
      RandomAccessFile s = new RandomAccessFile(filename, "r");

/***

      byte[] buffer = new byte[10];

      System.out.println(s.length());
      s.read(buffer, 0, 1);
      System.out.println(s.length());
      s.close();
      System.out.println(s.length());
      s.close();

      System.out.println(s.available());
      s.read(buffer, 0, 1); System.out.println((char)buffer[0]);
      
      System.out.println(s.skip(1000000));

      buffer = new byte[s.available()];
      System.out.println(s.read(buffer, 0, buffer.length - 1));
      System.out.println(s.read(buffer, 0, 1));
      System.out.println(s.read(buffer, 0, 1));
      System.out.println(s.read(buffer, 0, 1));
***/
      
/**
      Double s = Double.valueOf("abc");
      System.out.println(s);
**/

/**
      int    src[] = { 1, 2, 3 };
      double dst[] = { 4, 5, 6 };
      System.arraycopy(src, 1, dst, 1, 2);
      System.arraycopy("abc", 1, "def", 1, 2);
      System.out.println(dst);
**/
      }
   }
