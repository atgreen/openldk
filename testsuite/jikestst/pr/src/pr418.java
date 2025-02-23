// $Id: pr418.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class MyClass {

	public void print(Object o) {
		System.out.println("Printing Object: " + o);
	}

	private void print(Integer i) {
		System.out.println("Printing Integer: " + i);
	}

	public void trampoline(Integer i) {
		print(i);
	}

}

public class Test {

	static void main(String[] args) {
		MyClass d = new MyClass();
		Integer i = new Integer(666);
		d.print((Object)i);
		d.print(i);
		d.trampoline(i);
	}

}

