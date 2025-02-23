// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
    public static void main(String args[]) {
        int i, s;
        s = 0;
        for (i = 0; i < 10; i++) {
            s = s + i;
        }

	System.out.println(s);
        if (s == 45) {
            System.exit(0);
        } else {
            System.exit(1);
        }
    }
}
