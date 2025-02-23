// This software is subject to the terms of the IBM Jikes Test Suite
// License Agreement available at the following URL:
// http://www.ibm.com/research/jikes.
// Copyright (C) 1996, 1999, International Business Machines Corporation
// and others.  All Rights Reserved.
// You must accept the terms of that agreement to use this software.

class Bottle implements cost, foreign {
  int costofitem;
  String madeIn;
  String languagesOnLabel[];

  Bottle(int i, String country,  String langs[]) {
    costofitem = i;
    madeIn = country;
    languagesOnLabel = langs;
   }

  public int price() {
    return costofitem;
  }

  public boolean hasEnglishLabel() {
    int i;
    for (i=0; i< languagesOnLabel.length; i++) {
     if (languagesOnLabel[i].equals("English")) return true;
    }
    return false;
  }

  public String getCountryName() {
    return madeIn;
  }

}
