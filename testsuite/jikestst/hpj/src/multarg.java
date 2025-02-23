// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public final class Test {
//  public static int i;
  public static void main(String argv[]) {
    int i = ifunc(12, 10, true, 'c', (short)15, false, "a", "bc", 9,
                  true, false, true, (byte)4, "abc", 'd', 'e', (short)2, 99,
                  1.45, 3.9, true);
    System.out.println(i);
    System.exit(i);
   }
  static int ifunc(int ii, int ij, boolean b, char c, short ss, boolean b2,
                    String x1, String x2, int ik, boolean b3, boolean b4,
                    boolean b5, byte ib, String x3, char c2, char c3,
                    short s2, int il, double f1, double f2, boolean b6) {
    if (!b || (c != 'c')) return ii - 1;
    int isum = 0;
    isum = isum + ii + ij + ss + ik + ib + s2 + il;
    System.out.println(isum);
    if (isum != (52 + 99)) return ii - 3;
    isum = 0;

    if (b & !b2 & b3 & !b4 & b5 & b6) isum  = isum + 1;   // should be executed

    if (f1 + f2 != 5.35) ij = 20;  // should not be executed

    if (x1.equals("a")) isum = isum + 1;
    if (x2 == "bc") isum = isum + 1;
    String xx = "abc";
    if (xx == x3) isum = isum + 1;

    if (isum != 4) return 5;
    return ii + 1 + ij;

  }
}

