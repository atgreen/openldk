// $Id: pr303b.java,v 1.2 1999/11/04 14:59:46 shields Exp $
// This software is subject to the terms of the IBM Jikes Compiler
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

interface F {
  interface G extends A.T {
  interface D { }
}
}

interface A extends B.G {
interface T extends F.G.D { }
}

interface B extends F {
interface X extends A.D { }
}
