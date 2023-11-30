/*************************************************************************
/* Test.java -- Test Writer
/*
/* Copyright (c) 1998, 2004 Free Software Foundation, Inc.
/* Written by Daryl Lee (dolee@sources.redhat.com)
/* And Mark Wielaard (mark@klomp.org)
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

package gnu.testlet.java.io.Writer;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.Writer;
import java.io.IOException;

public class Test extends Writer implements Testlet
{

	private static final int LEN = 100;
	private int index;
	private char[] buf;

	public Test()
	{
		super();
		buf = new char[LEN];
		index = 0;
	}

	Test(Object lock)
	{
		super(lock);
	}

	public void write(char cbuf[], int off, int len)
		throws IOException
	{
		for (int i = 0; i < len; i++) {
			buf[index++] = cbuf[off + i];
		}
	}

	public void flush() throws IOException
	{
		// nothing to do
	}

	public void close() throws IOException
	{
		// nothing to do
	}

	public String toString()
	{
		return new String(buf, 0, index);
	}

public void 
test(TestHarness harness)
{
  Test tw = new Test();
  char[] buff = {'A', 'B', 'C', 'D'};
  try {  // Just one block for all possible IOExceptions
	tw.write('X');						// X
	harness.check(true, "write(int)");
	tw.write(buff);						// XABCD
	harness.check(true, "write(buf)");
	tw.write(buff, 1, 2);				// XABCDBC
	harness.check(true, "write(buf, off, len)");
	tw.write("YZ");						// XABCDBCYZ
	harness.check(true, "write(string)");
	tw.write("abcde", 2, 2);			// XABCDBCYZcd
	harness.check(tw.toString(), "XABCDBCYZcd", "All Characters written okay");
  }
  catch (IOException e) {
	harness.fail("Unexpected IOException");
  }

  // The lock object must be non-null.
  boolean npe_thrown = false;
  try {
	new Test(null);
  }
  catch (NullPointerException npe) {
	  npe_thrown = true;
  }
  harness.check(npe_thrown, "null lock object");
}

} // class Test

