// $Id: pr141.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class ThrowTest {
	public ThrowTest ()
	{
		String s = throwerr();
	}

	static final public String throwerr() {
/* REMOVE THE 10 LINES BELOW AND IT WORKS */
		int x = 0;
label_1:
		while (true) {
			switch (x) {
				case 1:
					break label_1;
				default:
					x++;
			}
		}
/* REMOVE THE 10 LINES ABOVE AND IT WORKS */

		{if (true) return ("Test");}
		throw new Error("Missing return statement in function");

/* REPLACE ABOVE TWO LINES WITH THE 2 LINES BELOW AND IT WORKS
        if (true) return ("Test"); else
        throw new Error("Missing return statement in function");
*/
	}


	public static void main(String[] args) {
		ThrowTest t__ = new ThrowTest();
		System.out.println("0");
	}
}


