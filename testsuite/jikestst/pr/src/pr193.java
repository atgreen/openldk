// $Id: pr193.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Tx
{
	final int a;

	// Instance init
	{
		try {
			a = 4;
		}
		finally{
			a = 5; //Ouch re init of a final
		}
	}
}


