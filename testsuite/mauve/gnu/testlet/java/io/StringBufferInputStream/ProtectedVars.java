/*************************************************************************
/* ProtectedVars.java -- StringBufferInputStream protected variables
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

package gnu.testlet.java.io.StringBufferInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class ProtectedVars extends StringBufferInputStream implements Testlet
{

public
ProtectedVars(String b)
{
  super(b);
}

// The constructor for the test suite.
public
ProtectedVars ()
{
  super("");
}

public void
test(TestHarness harness)
{
  String str = "Between my freshman and sophomore years of high school\n" +
    "we moved into a brand new building.  The old high school was turned\n" +
    "into an elementary school.\n";

  ProtectedVars sbis = new ProtectedVars(str);
  byte[] read_buf = new byte[12];

  try 
    {
      sbis.read(read_buf);
      sbis.mark(0);
    
      sbis.read(read_buf);
      harness.check(sbis.pos, read_buf.length * 2, "pos");
      harness.check(sbis.count, str.length(), "count");
      harness.check(sbis.buffer, str, "buf");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

}

