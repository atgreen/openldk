// $Id: pr373.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.util.Enumeration;
import javax.swing.tree.TreeNode;
import com.sun.xml.tree.ElementNode;

public class ShowElement extends ElementNode implements TreeNode {
	public ShowElement () {
	};

	public boolean isLeaf () {
		return !hasChildNodes();
	}

	public TreeNode getChildAt (int n) {
		return null;
	}

	public int getChildCount () {
		return getLength ();
	}

	public TreeNode getParent () {
		return (TreeNode) getParentNode ();
	}

	public int getIndex (TreeNode node) {
		return -1;
	}

	public boolean getAllowsChildren () {
		return true;
	}

	public Enumeration children () {
		throw new RuntimeException ("NYI");
	}

}
