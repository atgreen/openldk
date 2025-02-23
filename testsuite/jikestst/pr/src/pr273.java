// $Id: pr273.java,v 1.6 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// pr273 -- name omitted from method argument list
class Test {
  public void foo(String) {}
}
interface A { void m(Object a); }
interface B { void m(String a); }
interface C extends A, B { }

public class Main implements C {
	public static void main(String argv[]) {
		C C = new Main();
		((B) C).m("A String");
		C.m("A String");
	}
	public void m(Object o) { }
	public void m(String o) { }
}
