// $Id: pr120c.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class C extends B {
	public static void main(String args[]) {
		C c = new C();
		c.amethod();
		c.hello();
	}

	public void amethod() {
		System.out.println("amethod");
	}
}
