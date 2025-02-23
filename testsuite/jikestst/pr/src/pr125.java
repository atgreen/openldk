// $Id: pr125.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

package jalama;

public interface DbTypes
{
	static final Thing SECT = new Thing(0);
	static final Thing SHIP = new Thing(1);
	static final Thing PLANE = new Thing(2);
	static final Thing UNIT = new Thing(3);
	static final Thing NUKE = new Thing(4);

	final class Thing 
	{
		private final int thing;
		private Thing( int i )
		{
			thing = i;
		}
	}
}
