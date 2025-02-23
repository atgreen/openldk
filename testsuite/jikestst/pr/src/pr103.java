// $Id: pr103.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.Serializable;

public class TestSer {

	public static void main(String[] args) {
		Integer[] s={new Integer(1), new Integer(2)};

		Serializable t = s;
  System.out.println("0");
	}
}
