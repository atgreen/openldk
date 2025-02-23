// $Id: pr363.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Constant1 {
	void v(int p) {
		switch (p) {
			case 7:
			case Constant2.i:
				break;
		}
	}
	static final int j = Constant2.k * 3;
}
class Constant2 {
	static final int i = Constant1.j + 1;
	static final int k = 2;
}
