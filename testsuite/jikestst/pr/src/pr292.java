// $Id: pr292.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

// default constructor for cns1a calls a missing super-constructor
class cns1 {
	cns1(Object o) {
	}
}
class cns1a extends cns1 {
}

