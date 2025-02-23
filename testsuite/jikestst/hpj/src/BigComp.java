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
long a, b;
int c;
double d,e;
d = 25.5;
e = 30.2;
a = 25;          
b = 30;
if (a <= b) c = 1;
else c = 0;
if (e > d)  c = c + 11;
System.out.println(c);
System.exit(c);
}
}

