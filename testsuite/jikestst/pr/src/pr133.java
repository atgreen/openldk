// $Id: pr133.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class neil1 {
	static public class A{
		int a;
		private A(int i){ a = i;}
		A(){}
		public A(int i, float f){ a = i;}
	}
 public static void main(String[] args) {
  System.out.println("0");
 }



}

