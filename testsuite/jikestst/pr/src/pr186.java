// $Id: pr186.java,v 1.5 1999/11/04 14:59:44 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Test
{
	boolean fx = true;
	int i_state = 0;
	class T {
		T() throws IllegalAccessException {
			if (fx)
				throw new IllegalAccessException("zap from T");
			fx = false;
		}
	}


	T t2;

	/* init */{
		try {
			t2 = new T();
			i_state = 1;
		}catch(Exception e) {
			i_state += 10;
		}finally {
			i_state += 100;
		}
	}
 public static void main(String[] args) {
  System.out.println("0");
 }
}

