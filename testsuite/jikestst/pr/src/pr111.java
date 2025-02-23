// $Id: pr111.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// This is an inner class of ItemNode.java:
private static class ChildEnumeration implements Enumeration {
	private int i=0;

  //---------------- the part in question: ------------------
	private final Vector list;
  /**
   * @param the non-null vector
   */
	ChildEnumeration(Vector l) {
		if (l==null) throw
				new IllegalArgumentException("l must be non-null!");

		this.list = l;
	}
  //----------------------------------------------------------

	public boolean hasMoreElements() {
		return ( i<list.size() );
	}
	public Object nextElement() {
		if (hasMoreElements()) {
			return list.elementAt(i++);
		} else {
			throw new java.util.NoSuchElementException("No more elements");
		}
	}
}

