// $Id: pr350.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// 
//  Argument passing problem
//  gmt@cs.arizona.edu / 5-Feb-1999
//
//  Jikes 0.42 / X86  (also fails on 0.40, 0.41)
//  Red Hat Linux 5.1
//
//  This program should output four identical lines.
//  Instead, it outputs:
//	111 222 3.3
//	111 222 3.3
//	111 222 3.3
//	111 953482739712 1.1E-321
//
//  Repeat by:
//	jikes argbug.java
//	java argbug
//
//  (output is correct if compiled by javac instead)



public class argbug {


	public static void main(String[] args) {

		print1(111, 222, 3.3);			// ok
		print2(111, 222, 3.3);			// ok

		new Thread() {
			public void run() {
				print1(111, 222, 3.3);		// ok
				print2(111, 222, 3.3);		// prints wrong 2nd/3rd args
			}
		}.start();
	}



//  public method always prints correct values

	public static void print1(long a, long b, double c) {
		System.out.println(a + " " + b + " " + c);
	}



//  private method sometimes prints wrong values

	private static void print2(long a, long b, double c) {
		System.out.println(a + " " + b + " " + c);
	}



} // class argbug

