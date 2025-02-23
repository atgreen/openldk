// $Id: pr278.java,v 1.5 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;

class DATest
{
	DATest (File javaFile)
	{
		try
		{
			_istream = new FileInputStream(javaFile);
			// _istream is DA here
		}
		catch (FileNotFoundException e)
		{
			throw new IllegalStateException();
			// and here
		}
		// and therefore here
	}

	private final FileInputStream _istream;
	// must be DA by end of	constructor
}


