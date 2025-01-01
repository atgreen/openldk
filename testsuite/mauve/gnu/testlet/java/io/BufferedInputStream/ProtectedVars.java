/*************************************************************************
/* ProtectedVars.java -- Tests BufferedInputStream protected variables
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

// Tags: JDK1.0

package gnu.testlet.java.io.BufferedInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class ProtectedVars extends BufferedInputStream implements Testlet
{

public
ProtectedVars(InputStream in, int size)
{
  super(in, size);
}

// Constructor for test suite
public
ProtectedVars()
{
  super(System.in);
}

public void
test(TestHarness harness)
{
  try
    {
      String str = "This is a test line of text for this pass";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      ProtectedVars bist = new ProtectedVars(sbis, 12); 

      bist.read();
      bist.mark(5);

      harness.check(bist.buf.length, 12, "buf.length");
      harness.check(bist.count, 12, "count");
      harness.check(bist.marklimit, 5, "marklimit");
      harness.check(bist.markpos, 1, "markpos");
      harness.check(bist.pos, 1, "pos");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
} // main

} // class ProtectedVars

