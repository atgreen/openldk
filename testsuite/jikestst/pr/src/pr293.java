// $Id: pr293.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class n4 {
	static void m() {}
	static void m(int x, double k) {}
	static void m(int i) {}
	static void m(boolean b) {}
	static void m(double d) {}
	{
		m(1.0);
	}
	static class n44 {
		static void m() {}
		void v() {
			m(1.0);
		}
	}
}

