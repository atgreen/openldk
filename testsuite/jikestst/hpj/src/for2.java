// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
    public static void main(String args[]) {
        int i, j, s;
        s = 0;
        for (i = 0; i < 10; i++) {
            for (j = 0; j < 15; j++) {
                s = s + i + j;
            }
        }

	System.out.println(s);
        if (s == 1725) {
            System.exit(0);
        } else {
            System.exit(1);
        }
    }
}
