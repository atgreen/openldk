// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
    public static void main ( String args[] ) {
        int i;
	long l;

        l = 0x0000FFFF40000000L;
	i = (int)l;
        System.out.println((byte)(i >> 30));
        System.exit((byte)(i >> 30));
    }
}
