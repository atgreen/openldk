/*************************************************************************
/* Test.java -- Test StringWriter
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

package gnu.testlet.java.io.StringWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

/**
  * Class to test StringWriter. This is just a rehash of the
  * BufferedCharWriterTest using a StringWriter instead of a 
  * CharArrayWriter underneath.
  */
public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  StringWriter sw = new StringWriter();

  String str = "There are a ton of great places to see live, original\n" +
    "music in Chicago.  Places like Lounge Ax, Schuba's, the Empty\n" +
    "Bottle, and even the dreaded Metro with their sometimes asshole\n" +
    "bouncers.\n";


  char[] buf = new char[str.length()];
  str.getChars(0, str.length(), buf, 0);

  sw.write(buf, 0, 5);
  sw.write(buf, 5, 8);
  sw.flush();
  harness.check(sw.toString(), str.substring(0, 13), "flush()");
  harness.check(sw.getBuffer().toString(), str.substring(0, 13), "getBuffer()");
  sw.write(buf, 13, 12);
  sw.write(buf[25]);
  sw.write(buf, 26, buf.length - 26);
  try		// IOException added in 1.2
    {
      sw.close();
    }
  catch(Throwable t)
    {
      harness.debug("Caught unexpected exception: " + t);
      harness.fail("Unexpected Throwable");
      return;
    }

  harness.debug(sw.toString());
  harness.check(sw.toString(), str, "string equality");
}

} // class Test

