// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// %GROUP AbsClass0 AbsClass1
// %TSR EE 6
// %MAIN

import java.lang.*;

class AbsClassMain {
    public static void main(String args[]) {
        AbsClass0 x = new AbsClass1();
        int i = x.foo(5);
        System.out.println(i);
        System.exit(i);
    }
}
