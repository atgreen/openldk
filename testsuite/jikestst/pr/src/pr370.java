// $Id: pr370.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class Foo {
	public static Object test() {
		Object foo = new Object();
		try {
			synchronized(foo) {
		 /* A */         return foo;
		 /* B */     }
		} finally {
			boolean b = false;
			System.out.println("foo is " + foo);
		}
	}

	public static void main(String argv[]) {
		System.out.println("test returns " + test());
	}
}
