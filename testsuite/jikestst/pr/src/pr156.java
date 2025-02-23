// $Id: pr156.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// Problem 155
// reported by K. Zadeck, 07 June 98
// both jikes and javac reject, jikes with caution, javac with error
// problem is that jikes produces class file that fails verification
class bug156 {
	static void m(int SmallMethodSize, boolean bigInline)
	{


		int desire;
		int FullValue = 20;
		if (bigInline) {
			int value = 10;
			if (value > (FullValue / 2)) {
				desire  = 1;
			}
			else if (value > (FullValue / 4)) {
				desire  = 2;
			}
			else if (value > (FullValue / 8)) {
				desire  = 3;
			}
			else if (value > (FullValue / 16)) {
				desire  = 4;
			} else desire = 5;

		}

		int ic=0;
		for (ic = 0;ic <10; ic++) {
			if (bigInline) {
				System.out.println(desire * 10);
			} 
		}

	}
}
