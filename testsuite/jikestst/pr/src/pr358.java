// $Id: pr358.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class T {
  Object thrower () throws Exception {
    int i=1,j=2;
    if (i==j) {
      throw new Exception(); 
    }
    return new Object(); 
  }
  void m() {
    final Object o;
    Object defaultValue = new Object();
    try {
    	o = thrower();
    } catch (Exception e) {
    	o = defaultValue;  // declared elsewhere
    }
  }
}
