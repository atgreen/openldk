// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

import java.io.*;
import java.util.*;
import java.lang.*;

class Test {
	public static void main (String argv[]) {
ques ques = new ques();
int a,b,c,d, e;
int accum = 0;
b = 5;
c = 7;
d = 9;
a = ques.multy_g(b,c,d);
e = ques.multy_g(c,b,d);
if ((e == 16) && (a == 16)); else accum++;

a = ques.multy_l(b,c,d);
e = ques.multy_l(c,b,d);
if ((e == 14) && (a == 14)); else accum+=2;

a = ques.multy_le(b,c,d);
e = ques.multy_le(c,b,d);
if ((e == 14) && (a == 14)); else accum+=4;

a = ques.multy_ge(b,c,d);
e = ques.multy_ge(c,b,d);
if ((a == 16) && (e == 16)); else accum+=8;

a = ques.multy_e(b,b,d);
e = ques.multy_e(c,b,d);
if ((a == 18) && (e == 14)); else accum+=16;

a = ques.multy_n(b,c,d);
e = ques.multy_n(b,b,d);
if ((a == 18) && (e == 14)); else accum+=32;
System.out.println(accum);
System.exit(accum);
}
}

