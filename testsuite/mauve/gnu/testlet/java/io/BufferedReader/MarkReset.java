/*************************************************************************
/* MarkRest.java -- Tests BufferedReader mark/reset functionality
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

package gnu.testlet.java.io.BufferedReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class MarkReset extends CharArrayReader implements Testlet
{

// Hehe.  We override CharArrayReader.markSupported() in order to return
// false so that we can test BufferedReader's handling of mark/reset in
// both the case where the underlying stream does and does not support
// mark/reset
public boolean
markSupported()
{
  return(false);
}

public
MarkReset(char[] buf)
{
  super(buf);
}

// Constructor for test suite
public
MarkReset()
{
  super(new char[1]);
}

public static int
marktest(Reader ins, TestHarness harness) throws IOException
{
  BufferedReader br = new BufferedReader(ins, 15);

  int chars_read;  
  int total_read = 0;
  char[] buf = new char[12];

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  br.mark(75);
  br.read();
  br.read(buf);
  br.read(buf);
  br.read(buf);
  br.reset();

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  br.mark(555);

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  br.reset();

  br.read(buf);
  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  br.mark(14);

  br.read(buf);

  br.reset();

  chars_read = br.read(buf);
  total_read += chars_read;
  harness.debug(new String(buf, 0, chars_read), false);

  while ((chars_read = br.read(buf)) != -1)
    {
      harness.debug(new String(buf, 0, chars_read), false);
      total_read += chars_read;
    }

  return(total_read);
}

public void
test(TestHarness harness)
{
  try
    {
      harness.debug("First mark/reset test series");
      harness.debug("Underlying reader supports mark/reset");

      String str = "Growing up in a rural area brings such delights.  One\n" +
        "time my uncle called me up and asked me to come over and help him\n" +
        "out with something.  Since he lived right across the field, I\n" +
        "walked right over.  Turned out he wanted me to come down to the\n" +
        "barn and help him castrate a calf.  Oh, that was fun.  Not.\n";

      StringReader sr = new StringReader(str);
      BufferedReader br = new BufferedReader(sr);

      int total_read = marktest(br, harness);
      harness.check(total_read, str.length(), "total_read");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
   
  try
    {
      harness.debug("Second mark/reset test series");
      harness.debug("Underlying reader does not support mark/reset");

      String str = "Growing up we heated our house with a wood stove.  That\n" +
        "thing could pump out some BTU's, let me tell you.  No matter how\n" +
        "cold it got outside, it was always warm inside.  Of course the\n" +
        "downside is that somebody had to chop the wood for the stove. That\n" +
        "somebody was me.  I was slave labor.  My uncle would go back and\n" +
        "chain saw up dead trees and I would load the wood in wagons and\n" +
        "split it with a maul. Somehow my no account brother always seemed\n" +
        "to get out of having to work.\n";

      char[] buf = new char[str.length()];
      str.getChars(0, str.length(), buf, 0);
      MarkReset mr = new MarkReset(buf);
      BufferedReader br = new BufferedReader(mr);

      int total_read = marktest(br, harness);
      harness.check(total_read, str.length(), "total_read");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
} // main

} // class MarkReset

