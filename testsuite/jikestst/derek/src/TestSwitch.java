// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// TestTableswitch

class Test
   {
   public static void main(String args[])
      {
      int j;
      
      // tableswitch
      System.out.print("\nwant: 99101199\n got: ");
      for (int i = 9; i < 13; i += 1)
         {
         switch (i)
            {
            case 10: j = 10; break;
            case 11: j = 11; break;
            default: j = 99; break;
            }
         System.out.print(j);
         }
      System.out.println();
      
      // lookupswitch
      System.out.print("\nwant: 9910203099\n got: ");
      for (int i = 0; i < 50; i += 10)
         {
         switch (i)
            {
            case 10: j = 10;  break;
            case 20: j = 20;  break;
            case 30: j = 30;  break;
            default: j = 99;  break;
            }
         System.out.print(j);
         }
      System.out.println();
      }
   }
