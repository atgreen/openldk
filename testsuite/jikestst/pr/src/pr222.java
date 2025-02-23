// $Id: pr222.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test {
	void m(int i) {
switch ( i )
{
	case: 1
		  {
			System.out.println("1");
		  }
		  break;

	case: 2 // Erroneous case
		  {
		  } // extra close brace, this caused Jikes to crash.
}
break;
}
}
