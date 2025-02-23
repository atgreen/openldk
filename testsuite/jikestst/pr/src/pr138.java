// $Id: pr138.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


final class Main {
	int x;


	void testBad() {
		if (0 < this.x) {
			System.err.println("0 < " + this.x);
		} else {
			System.err.println("0 not < " + this.x);
		}
	}

	void testGood() {
		if (this.x > 0) {
			System.err.println("0 < " + this.x);
		} else {
			System.err.println("0 not < " + this.x);
		}
	}

	public static void main(String[] s) {
		Main m = new Main();

		m.x = -1;
		m.testBad();
		m.testGood();

		m.x = 0;
		m.testBad();
		m.testGood();

		m.x = 1;
		m.testBad();
		m.testGood();
	}
}

