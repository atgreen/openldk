// $Id: pr203.java,v 1.6 1999/11/04 14:59:45 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class testclass extends Object {

	static int maxRedirects = 5;

	static boolean alwaystrue() {
		return true;
	}

	public static void main( String args[]  ) {

		int redirects = 0;
		do {


			if (alwaystrue() ) {
				redirects++;
				continue;
			}

			System.out.println( "short!" );
			return;
		} while (redirects < maxRedirects);

		System.out.println( "done!  jikes says we can't get here." );
	}

