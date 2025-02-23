// $Id: pr406.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class JikesBug
{
	public static void
			main(String[] argv) {
		(new JikesBug()).doIt();
	}

	public void
			doIt() {
		System.out.println("Using OKAccessor");
		(new OKAccessor()).doIt();
		System.out.println("Using BrokenAccessor");
		(new BrokenAccessor()).doIt();
	}

	private String
			method() {
		return "Hello World!";
	}

	private class OKAccessor
	{
		public void
				doIt() {
			String str = method();
			String upper = str.toUpperCase();
			System.out.println(upper);
		}
	}

	private class BrokenAccessor
	{
		public void
				doIt() {
			String str = method().toUpperCase();
			System.out.println(str);
		}
	}
}

