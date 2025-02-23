// $Id: pr291.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// pr 291
class A
{
	{
		System.out.println("Class A");
	}
}

class B extends A
{
	{
		System.out.println("Class B");
	}
}

class C
{
	public static void main(String[] args)
	{
		B b = new B();
	}
}
