// $Id: pr146.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// check that needed conversion from int to long done on return from synchronized statement
class Test {
	public static void main(String[] args) {
		Test z = new Test();
		System.out.println(z.m(5));
	}
	synchronized long m(int j) {
		byte b1=0,b2=1;
		Object synch = new Object();
		synchronized(synch) {
			return (b1<<3) + (b2<<4);
		}
	}
}
