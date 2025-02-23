// $Id: pr287.java,v 1.5 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class B {
	public int f() {
		try {
			throw new ArrayIndexOutOfBoundsException("test");
		} catch(StringIndexOutOfBoundsException e1) {
			System.out.println("catch StringIndexOutOfBoundsException");
			throw new RuntimeException("hello");
		}
		catch(ArrayIndexOutOfBoundsException e2) {
			System.out.println("catch ArrayIndexOutOfBoundsException");
			if(e2 != null) // some senseless check ...
				return 0;
		}
		finally {
			System.out.println("finally");
		}

		return 1;
	}

	public static void main(String[] args) {
		B b = new B();
		System.out.println(b.f());
	}
}

