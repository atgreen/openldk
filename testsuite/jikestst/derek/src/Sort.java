// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class Sort
   {
   public static void
   main(String args[])
      {
      if (args == null)
         System.out.println("specify an integer (= number of items to sort)");
      else
         new Sort().test(Integer.parseInt(args[0]));
      }
      
   void
   test(int length)
      {
      int list[] = new int[length];

      int count = list.length;
      for (int i = 0; i < list.length; ++i)
         list[i] = --count;

      sort(list);
      
//    for (int i = 0; i < list.length; ++i)
//       System.out.println(i);
      
      }
   
   // Shell sort.
   //
   void
   sort(int x[])
      {
      int n = x.length;
      int acount = 0;
      int bcount = 0;
      int ccount = 0;
      for (int gap = n / 2; gap > 0; gap /= 2)
         {
         ++acount;
         for (int i = gap; i < n; ++i)
            {
            ++bcount;
            for (int j = i - gap; j >= 0; j -= gap)
               {
               ++ccount;
               if (x[j] < x[j + gap])
                  break;
               swap(x, j, j + gap);
               }
            }
         }
      System.out.println(acount);
      System.out.println(bcount);
      System.out.println(ccount);
      }
      
   // Interchange "x[i]" with "x[j]"
   //
   void
   swap(int x[], int i, int j)
      {
      int tmp = x[i];
      x[i] = x[j];
      x[j] = tmp;
      }
   }
