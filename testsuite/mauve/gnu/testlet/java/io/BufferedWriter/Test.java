/*************************************************************************
/* Test.java -- Test {Buffered,CharArray}Writer
/*
/* Copyright (c) 1998, 1999 Free Software Foundation, Inc.
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

package gnu.testlet.java.io.BufferedWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class Test implements Testlet
{

public void 
test(TestHarness harness)
{
  try
    {
      CharArrayWriter caw = new CharArrayWriter(24);
      BufferedWriter bw = new BufferedWriter(caw, 12);

      String str = "I used to live right behind this super-cool bar in\n" +
        "Chicago called Lounge Ax.  They have the best music of pretty\n" +
        "much anyplace in town with a great atmosphere and $1 Huber\n" +
        "on tap.  I go to tons of shows there, even though I moved.\n";

      char[] buf = new char[str.length()];
      str.getChars(0, str.length(), buf, 0);

      bw.write(str.substring(0, 5));   // write(String)
      harness.check(caw.toCharArray().length, 0, "buffering/toCharArray");
      bw.write(buf, 5, 8);
      bw.write(buf, 13, 12);
      bw.write(buf[25]);
      bw.write(buf, 26, buf.length - 27);
	  bw.newLine();					   // newLine()
	  bw.flush();
      bw.close();

      String str2 = new String(caw.toCharArray());
      harness.check(str, str2, "Did all chars make it through?");
      harness.debug(str2, false);
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false, "Caught unexpected exception");
    }
}

} // class Test

