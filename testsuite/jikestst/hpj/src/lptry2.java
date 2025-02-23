// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
    public static void main(String args[]) {
        int i, j;
        int s[] = new int[7];
        for (i = 0; i < 10; i++) {
            try {
                s[i] = i;
            } catch (Exception e) {
                System.out.println(0);
                System.exit(0);
            }
        }
        System.exit(1);
        System.out.println(1);
    }
}
