/*************************************************************************
/* Test.java -- Test java.text.ParsePosition
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

// FIXME: This test should be split into two.
// getErrorIndex is a JDK1.2 method.  The rest were present in 1.1.

package gnu.testlet.java.text.ParsePosition;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.text.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  ParsePosition pp = new ParsePosition(69);
  harness.check(pp.getIndex(), 69, "getIndex() post-create");

  pp.setIndex(666);
  harness.check(pp.getIndex(), 666, "set/getIndex()");

  harness.check(pp.getErrorIndex(), -1, "getErrorIndex() no error");

  pp.setErrorIndex(65536);
  harness.check(pp.getErrorIndex(), 65536, "set/getErrorIndex()");

  harness.debug(pp.toString());
}

} // class Test

