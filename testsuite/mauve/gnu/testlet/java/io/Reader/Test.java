/*************************************************************************
/* Test.java -- Test Reader
/*
/* Copyright (c) 1998 Free Software Foundation, Inc.
/* Written by Daryl Lee (dolee@sources.redhat.com)
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

// Tags: JDK1.1

package gnu.testlet.java.io.Reader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.Reader;
import java.io.IOException;

public class Test extends Reader implements Testlet
{

	private int index;
	private String s;

	public Test() { }

	Test(String str)
	{
		super();
		s = str;
		index = 0;
	}

	public int read(char cbuf[], int off, int len)
		throws IOException
	{
		int i;
		for (i = 0; i < len; i++) {
			cbuf[off + i] = s.charAt(index++);
		}
		return (i);
	}

	public void close() throws IOException
	{
		// nothing to do
	}

public void 
test(TestHarness harness)
{
  String str = "There are a ton of great places to see live, original\n" +
    "music in Chicago.  Places like Lounge Ax, Schuba's, the Empty\n" +
    "Bottle, and even the dreaded Metro with their sometimes asshole\n" +
    "bouncers.\n";

  Test sr = new Test(str);
  harness.check(true, "StringReader(String)");
  try {		// 1.1 API does not correctly document this exception
    harness.check(!sr.ready(), "ready()");
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException on ready()");
  }
  harness.check(!sr.markSupported(), "markSupported()");
  try {
    sr.mark(0);   // For this class, readahead limit should be ignored
	harness.fail("mark() should throw exception");
  }
  catch (IOException e) {
	harness.check(true, "mark()");
  }
  char[] buf = new char[4];
  try {
    harness.check(sr.read(buf, 0, 4), 4, "read(buf, off, len) return value");;
    String bufstr = new String(buf);
    harness.check(bufstr, "Ther", "read(buf, off, len)");
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException on read(buf, off, len)");
  }
  try {		// 1.1 API does not correctly document this exception
    sr.reset();
	harness.fail("Expected IOException on reset()");
  }
  catch (IOException e) {
	harness.check(true, "reset()");
  }
  try {
	sr.skip(8);
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException on skip()");
  }
  try {
	harness.check(sr.read(), 't', "skip(), read()");
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException on read()");
  }	
  try {
    sr.close();
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException on close()");
  }
  harness.check(true, "close()");
}

} // class Test

