// $Id: pr355a.java,v 1.2 1999/11/04 14:59:47 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

public class kernel {
    static public class AstList extends Lang.AstNode {
	public String name() { return("AstList"); }
    }

    static public class AstNode {
	public String name() { return("AstNode"); }
    }
}
