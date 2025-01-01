/*************************************************************************
/* MarkReset.java -- Tests BufferedInputStream mark for huge values
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

// Tags: JDK1.0

package gnu.testlet.java.io.BufferedInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class BigMark implements Testlet
{
  public void test(TestHarness harness)
  {
    test(harness, 32 * 1024);
    test(harness, 128 * 1024);
    test(harness, 1024 * 1024);
    test(harness, Integer.MAX_VALUE - 1024);
    test(harness, Integer.MAX_VALUE - 1);
    test(harness, Integer.MAX_VALUE);
  }

  public void test(TestHarness harness, int size)
  {
    harness.checkPoint("mark(" + size + ")");
    try
      {
	// array larger than default
	final int K = 16;
	byte[] dummy = new byte[K * 1024];
	dummy[2] = 42;
	dummy[3] = 13;
	ByteArrayInputStream bais = new ByteArrayInputStream(dummy);
	BufferedInputStream bis = new BufferedInputStream(bais);

	bis.read();
	bis.read();
	bis.mark(size);
	int answer = bis.read();
	harness.check(answer, 42);

	for (int i = 0; i < K / 2; i++)
	  bis.skip(1024);
	bis.reset();
	answer = bis.read();
	harness.check(answer, 42);

	bis.mark(size);
	answer = bis.read();
	harness.check(answer, 13);
	for (int i = 0; i < (K / 2) * 1024; i++)
	  bis.read();
	bis.reset();
	answer = bis.read();
	harness.check(answer, 13);
      }
    catch (IOException e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }
}
