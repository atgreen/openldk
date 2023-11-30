/*************************************************************************
/* Test.java -- Test java.text.FieldPosition
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Aaron M. Renn (arenn@urbanophile.com)
/*
/* This program is free software; you can redistribute it and/or modify
/* it under the terms of the GNU General Public License as published 
/* by the Free Software Foundation, either version 2 of the License, or
/* (at your option) any later version.
/*
/* This program is distributed in the hope that it will be useful, but
/* WITHOUT ANY WARRANTY; without even the implied warranty of
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/* GNU General Public License for more details.
/*
/* You should have received a copy of the GNU General Public License
/* along with this program; if not, write to the Free Software Foundation
/* Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307 USA
/*************************************************************************/

// Tags: JDK1.2

// FIXME: there should be a 1.1 version of this test.

package gnu.testlet.java.text.FieldPosition;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  FieldPosition fp = new FieldPosition(21);
  harness.check(fp.getField(), 21, "getField()");

  harness.check(fp.getBeginIndex(), 0, "getBeginIndex on create");
  harness.check(fp.getEndIndex(), 0, "getEndIndex on create");

  fp.setBeginIndex(1999);
  harness.check(fp.getBeginIndex(), 1999, "set/getBeginIndex");

  fp.setEndIndex(2001);
  harness.check(fp.getEndIndex(), 2001, "set/getEndIndex");

  FieldPosition fp2 = new FieldPosition(21);
  fp2.setBeginIndex(1999);
  fp2.setEndIndex(2001);
  harness.check(fp.equals(fp2) == true, "equals (true)");

  FieldPosition fp3 = new FieldPosition(1984);
  fp3.setBeginIndex(1999);
  fp3.setEndIndex(2001);
  harness.check(fp.equals(fp3) == false, "equals (false (pos diff))"); 

  fp3 = new FieldPosition(21);
  fp3.setBeginIndex(3000);
  fp3.setEndIndex(2001);
  harness.check(fp.equals(fp3) == false, "equals (false (beg diff))"); 

  fp3 = new FieldPosition(21);
  fp3.setBeginIndex(1999);
  fp3.setEndIndex(1984);
  harness.check(fp.equals(fp3) == false, "equals (false (end diff))"); 

  harness.debug(fp.toString());
}

} // class Test

