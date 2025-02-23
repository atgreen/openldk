// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestArrayAccess

class Test
   {
   public static void main(String args[])
      {
      boolean_array();
      byte_array();
      char_array();
      short_array();
      int_array();
      long_array();
      float_array();
      double_array();
      object_array();
      array_array();
      multi_int_array();
      multi_object_array();
      multi_partial_array();
      }

   static void
   boolean_array()
      {
      boolean array[] = new boolean[2];                                        // newarray type=4 eltsize=1
      boolean x0      = false;                                                 // iconst_0
      boolean x1      = true;                                                  // iconst_1

      array[0] = x0;                                                           // bastore
      array[1] = x1;                                                           // bastore

      System.out.print("\nwant: false\n got: "); System.out.println(array[0]); // baload
      System.out.print("\nwant: true\n got: ");  System.out.println(array[1]); // baload
      }

   static void
   byte_array()
      {
      byte array[] = new byte[2];                                              // newarray type=8 eltsize=1
      byte x0      = 127;
      byte x1      = -1;

      array[0] = x0;                                                           // bastore
      array[1] = x1;                                                           // bastore

      System.out.print("\nwant: 127\n got: "); System.out.println(array[0]);   // baload
      System.out.print("\nwant: -1\n got: ");  System.out.println(array[1]);   // baload (note sign extension)
      }

   static void
   char_array()
      {
      char array[] = new char[2];                                              // newarray type=5 eltsize=2
      char x0      = 0x7f41;
      char x1      = 0xff41;

      array[0] = x0;                                                           // castore
      array[1] = x1;                                                           // castore

      System.out.print("\nwant: A\n got: ");  System.out.println(array[0]);    // caload
      System.out.print("\nwant: A\n got: ");  System.out.println(array[1]);    // caload (note zero extension)
      }

   static void
   short_array()
      {
      short array[] = new short[2];                                            // newarray type=9 eltsize=2
      short x0      = 32767;
      short x1      = -1;

      array[0] = x0;                                                           // sastore
      array[1] = x1;                                                           // sastore

      System.out.print("\nwant: 32767\n got: "); System.out.println(array[0]); // saload
      System.out.print("\nwant: -1\n got: ");    System.out.println(array[1]); // saload (note sign extension)
      }

   static void
   int_array()
      {
      int array[] = new int[2];                                                // newarray type=10 eltsize=4
      int x0      = 0;
      int x1      = 1;

      array[0] = x0;                                                           // iastore
      array[1] = x1;                                                           // iastore

      System.out.print("\nwant: 0\n got: "); System.out.println(array[0]);     // iaload
      System.out.print("\nwant: 1\n got: "); System.out.println(array[1]);     // iaload
      }

   static void
   long_array()
      {
      long array[] = new long[2];                                              // newarray type=11 eltsize=8
      long x0      = 0;
      long x1      = 1;

      array[0] = x0;                                                           // lastore
      array[1] = x1;                                                           // lastore

      System.out.print("\nwant: 0\n got: "); System.out.println(array[0]);     // laload
      System.out.print("\nwant: 1\n got: "); System.out.println(array[1]);     // laload
      }

   static void
   float_array()
      {
      float array[] = new float[2];                                            // newarray type=6 eltsize=4
      float x0      = 0;
      float x1      = 1;

      array[0] = x0;                                                           // fastore
      array[1] = x1;                                                           // fastore

      System.out.print("\nwant: 0\n got: "); System.out.println(array[0]);     // faload
      System.out.print("\nwant: 1\n got: "); System.out.println(array[1]);     // faload
      }

   static void
   double_array()
      {
      double array[] = new double[2];                                          // newarray type=7 eltsize=8
      double x0      = 0;
      double x1      = 1;

      array[0] = x0;                                                           // dastore
      array[1] = x1;                                                           // dastore

      System.out.print("\nwant: 0\n got: "); System.out.println(array[0]);     // daload
      System.out.print("\nwant: 1\n got: "); System.out.println(array[1]);     // daload
      }

   static void
   object_array()
      {
      Object array[] = new Object[2];   // anewarray
      Object x0      = null;
      Object x1      = null;

      array[0] = x0;                    // aastore
      array[1] = x1;                    // aastore

      System.out.print("\nwant: null\n got: "); System.out.println(array[0]);     // aaload
      System.out.print("\nwant: null\n got: "); System.out.println(array[1]);     // aaload
      }

   static void
   array_array()
      {
      Object array[] = new Object[2];   // anewarray
      Object x0[]    = new Object[2];   // anewarray
      Object x1[]    = null;

      array[0] = x0;                    // aastore
      array[1] = x1;                    // aastore

      System.out.print("\nwant: [Ljava.lang.Object;\n got: "); System.out.println(array[0].getClass().getName()); // aaload
      System.out.print("\nwant: null\n got: ");                System.out.println(array[1]);                      // aaload
      }
   
   static void
   multi_int_array()
      {
      int outer  = 2;
      int middle = 3;
      int inner  = 4;
      
      int ary[][][] = new int[outer][middle][inner]; // multianewarray
      
      int n = 0;
      for (int i = 0; i < outer; ++i)
         for (int j = 0; j < middle; ++j)
            for (int k = 0; k < inner; ++k)
               ary[i][j][k] = n++;

      for (int i = 0; i < outer; ++i)
         for (int j = 0; j < middle; ++j)
            for (int k = 0; k < inner; ++k)
               System.out.println("ary["+i+"]["+j+"]["+k+"]="+ary[i][j][k]);

      System.out.println();
      }
   
   static void
   multi_object_array()
      {
      int outer  = 2;
      int middle = 3;
      int inner  = 4;
      
      Integer ary[][][] = new Integer[outer][middle][inner]; // multianewarray
      
      int n = 0;
      for (int i = 0; i < outer; ++i)
         for (int j = 0; j < middle; ++j)
            for (int k = 0; k < inner; ++k)
               ary[i][j][k] = new Integer(n++);

      for (int i = 0; i < outer; ++i)
         for (int j = 0; j < middle; ++j)
            for (int k = 0; k < inner; ++k)
               System.out.println("ary["+i+"]["+j+"]["+k+"]="+ary[i][j][k]);

      System.out.println();
      }
   
   static void
   multi_partial_array()
      {
      int outer  = 2;
      int middle = 3;
      
      int ary[][][] = new int [outer][middle][]; // multianewarray
      
      for (int i = 0; i < outer; ++i)
         for (int j = 0; j < middle; ++j)
            System.out.println("ary["+i+"]["+j+"]="+ary[i][j]);

      System.out.println();
      }
   }
