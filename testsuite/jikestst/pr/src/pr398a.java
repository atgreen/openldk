// $Id: pr398a.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class const1 {
	static final String s = "xyz";

	public static void main(String args[])
	{
		int i = 37;
		final String t = "pqr";

		switch (i) {
			case "abc" + const1.s + t + "" + 59 ==
					"def" + s ? 1 : 2:
					;
			case "abc" + true == "" ? 3 : 4:
				;
			case "abc" + 'x' == "" ? 5 : 6:
				;
			case "abc" + (byte)37 == "" ? 7 : 8:
				;
			case "abc" + (short)37 == "" ? 9 : 10:
				;
			case "abc" + 37 == "" ? 11 : 12:
				;
			case "abc" + 37L == "" ? 13 : 14:
				;
			case "abc" + 37.0f == "" ? 15 : 16:
				;
			case "abc" + 37.0 == "" ? 17 : 18:
				;
			case false + "abc" == "" ? 103 : 104:
				;
			case 'x' + "abc" == "" ? 105 : 106:
				;
			case (byte)37 + "abc" == "" ? 107 : 108:
				;
			case (short)37 + "abc" == "" ? 109 : 110:
				;
			case 37 + "abc" == "" ? 111 : 112:
				;
			case 37L + "abc" == "" ? 113 : 114:
				;
			case 37.0f + "abc" == "" ? 115 : 116:
				;
			case 37.0 + "abc" == "" ? 117 : 118:
				;
		}
	}
}

