// $Id: pr122b.java,v 1.5 1999/11/04 14:59:43 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.util.Hashtable;
class y
{
	// JLS 9.1 A compile time error if a name is the same by single-type-import
	class Hashtable{}
}
