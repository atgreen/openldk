// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


class Test {
    public static void main ( String args[] ) {
        int i;
	short s;

        i = 0xFFFF8000;
	s = (short)i;
        System.out.println(2 + (byte)(s >> 15));
        System.exit(2 + (byte)(s >> 15));
    }
}
