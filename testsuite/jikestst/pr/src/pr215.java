// $Id: pr215.java,v 1.5 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

interface Junk { public void junk(); }
public class External {
	public External() {
	}

	public Junk foo() { return new inner(); }
	private void bar() {
		System.out.println("bar");
	}

	class inner implements Junk {
		public void junk() {
			bar();
		}

	}


	public static void main(String[] args)
	{
		new External().foo().junk();

	}
}


