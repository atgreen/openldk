// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


class ques {
int x;
int multy_g(int w, int y, int z) {
x = z + ((w > y)?w:y);
return x;
}

int multy_l(int w, int y, int z) {
x = z + ((w < y)?w:y);
return x;
}

int multy_e(int w, int y, int z) {
x = z + ((w == y)?z:y);
return x;
}

int multy_n(int w, int y, int z) {
x = z + ((w != y)?z:y);
return x;
}

int multy_le(int w, int y, int z) {
x = z + ((w <= y)?w:y);
return x;
}

int multy_ge(int w, int y, int z) {
x = z + ((w >= y)?w:y);
return x;
}
}

