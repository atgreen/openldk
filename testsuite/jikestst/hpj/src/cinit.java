// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.


class cinit {
  static Fruit t1 = new Fruit();
  static { t1.seeds = 3; }
  static int i1 = 5 + Fruit.count;
  static int i2 = Basket.maxSize;
  Fruit t2 = new Fruit();
  int i3 = 15;

  cinit() {
    t2.seeds = 5;
    i3 = i3 + 1;
    i1 = i1 + 1;
  }
}
    

