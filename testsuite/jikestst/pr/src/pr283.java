// $Id: pr283.java,v 1.5 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

 // Test 1
    class T1 {
	      interface T1 {
		      int i = 10;
		      int j = this.i;
		      }
	 }

   
   
    // Test 2
    class T2 {
	      interface T2 {
		                public abstract abstract int m1();
		      }
	 }

   
    // Test 3
    class T3 {
	      // abstract method cannot be static
			      interface T3 {
		           static int m1();
		     }
