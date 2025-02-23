// $Id: pr361.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.util.Vector;

class SortedList extends List {
	SortedList() {
		super();
	}

	void add(int num) {
		ListElement newEl = new ListElement(num);
		boolean inserted = false;
		for (int i = 0, n = vec.size(); !inserted && i < n; i++) {
			ListElement curr = (ListElement)vec.elementAt(i);
			if (num < curr.num)
				vec.insertElementAt(newEl, i);
		}
		if (!inserted)
			vec.addElement(newEl);
	}
}

class List {
	Vector vec;

	class ListElement {
		final int num;
		ListElement(int _num) {
			num = _num;
		}
	}

	List() {
		vec = new Vector();
	}

	void add(int num) {
		vec.addElement(new ListElement(num));
	}
}
