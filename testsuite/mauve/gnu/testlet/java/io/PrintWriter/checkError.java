/*************************************************************************
/* Test.java -- Tests PrintWriter
/*
/* Copyright (c) 2004 Free Software Foundation, Inc.
/* Written by Mark Wielaard (mark@klomp.org)
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

package gnu.testlet.java.io.PrintWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

public class checkError extends OutputStream implements Testlet
{
  public void test(TestHarness harness)
  {
	// Check for (no) error after close
	PrintWriter p = new PrintWriter(new checkError());
	harness.check(!p.checkError());
	p.write("something");
	harness.check(!p.checkError());
	p.close();
	harness.check(!p.checkError());
	p.write("anotherthing");
	harness.check(p.checkError());
  }

  // Mini OutputStream implementation
  private boolean closed = false;
  public void close()
  {
    closed = true;
  }

  public void write(int i) throws IOException
  {
    if (closed)
      throw new IOException("closed stream");
  }

  public void flush() throws IOException
  {
    if (closed)
      throw new IOException("closed stream");
  }

} // class Test

