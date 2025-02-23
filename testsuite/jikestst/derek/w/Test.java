// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestReturn

class Test
   {
   public static void main(String args[])
      {
      /**/                                                         void_f();
      System.out.print("\nwant: true\n got: "); System.out.println(boolean_f());
      System.out.print("\nwant: 2\n got: ");    System.out.println(byte_f());
      System.out.print("\nwant: A\n got: ");    System.out.println(char_f());
      System.out.print("\nwant: 4\n got: ");    System.out.println(short_f());
      System.out.print("\nwant: 5\n got: ");    System.out.println(int_f());
      System.out.print("\nwant: 6\n got: ");    System.out.println(long_f());
      System.out.print("\nwant: 7\n got: ");    System.out.println(float_f());
      System.out.print("\nwant: 8\n got: ");    System.out.println(double_f());
      System.out.print("\nwant: null\n got: "); System.out.println(object_f());
      System.out.print("\nwant: null\n got: "); System.out.println(primitive_array_f());
      System.out.print("\nwant: null\n got: "); System.out.println(object_array_f());
      }

   static void     void_f()            {                           return  ; } // return
   static boolean  boolean_f()         { boolean x   = true;       return x; } // ireturn
   static byte     byte_f()            { byte    x   = 2;          return x; } // ireturn
   static char     char_f()            { char    x   = 0x41;       return x; } // ireturn
   static short    short_f()           { short   x   = 4;          return x; } // ireturn
   static int      int_f()             { int     x   = 5;          return x; } // ireturn
   static long     long_f()            { long    x   = 6;          return x; } // lreturn
   static float    float_f()           { float   x   = 7;          return x; } // freturn
   static double   double_f()          { double  x   = 8;          return x; } // dreturn
   static Object   object_f()          { Object  x   = null;       return x; } // areturn
   static int[]    primitive_array_f() { int     x[] = null;       return x; } // areturn
   static Object[] object_array_f()    { Object  x[] = null;       return x; } // areturn
   }
