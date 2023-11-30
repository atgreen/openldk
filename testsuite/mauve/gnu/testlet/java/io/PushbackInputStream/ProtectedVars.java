/*************************************************************************
/* ProtectedVars.java -- Test PushbackInputStream protected variables.
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

// Tags: JDK1.1

package gnu.testlet.java.io.PushbackInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class ProtectedVars extends PushbackInputStream implements Testlet
{

public
ProtectedVars(InputStream is, int size)
{
  super(is, size);
}

// For the test suite
public
ProtectedVars()
{
  super(null);
}

public void
test(TestHarness harness)
{
  String str = "Once when I was in fourth grade, my friend Lloyd\n" +
    "Saltsgaver and I got in trouble for kicking a bunch of\n" +
    "Kindergartners off the horse swings so we could play a game\n" +
    "of 'road hog'\n";

  ProtectedVars pv = new ProtectedVars(new StringBufferInputStream(str), 15);

  harness.check(pv.pos, pv.buf.length, "pst == buf.length");
  harness.check(pv.buf.length, 15, "buf.length");
}

} // class ProtectedVars

