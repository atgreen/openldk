/*************************************************************************
/* mark.java -- Tests LineNumberReader.mark() and reset().
/*
/* Copyright (c) 2003 Free Software Foundation, Inc.
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

package gnu.testlet.java.io.LineNumberReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class mark implements Testlet
{
  public void test(TestHarness harness)
  {
    String s = "1234567890abcdef";
    for (int nr = 0; nr <= 16; nr++)
      for (int limit = 1; limit < 16 - nr; limit++)
	{
	  String test = "nr: " + nr + " limit: " + limit;
	  try
	    {
	      StringReader sr = new StringReader(s);
	      LineNumberReader lnr = new LineNumberReader(sr, 2);
	      
	      // Read some nr of chars.
	      for (int i = 0; i < nr; i++)
		lnr.read();
	      
	      // Set limit and read char we want to return to.
	      lnr.mark(limit);
	      int j = lnr.read();

	      // Gobble up some more chars till the limit
	      for (int i = 0; i < limit - 1; i++)
		lnr.read();
	      
	      // Reset and reread char
	      lnr.reset();
	      int k = lnr.read();
	      
	      harness.check(j, k, test);
	    }
	  catch(IOException e)
	    {
	      harness.debug(e);
	      harness.check(false, test);
	    }
	}
  }
}

