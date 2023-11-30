/*************************************************************************
/* SimpleRead.java -- BufferedInputStream simple read test
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

public class SimpleRead implements Testlet
{

public void
test(TestHarness harness)
{
  try
    {
      String str = "One of my 8th grade teachers was Mr. Russell.\n" +
         "He used to start each year off by telling the class that the\n" +
         "earth was flat.  He did it to teach people to question\n" +
         "things they are told.  But everybody knew that he did it\n" +
         "so it lost its effect.\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      BufferedInputStream bis = new BufferedInputStream(sbis, 15);

      byte[] buf = new byte[12];
      int bytes_read, total_read = 0;
      while((bytes_read = bis.read(buf)) != -1)
        {
          harness.debug(new String(buf, 0, bytes_read), false);
          total_read += bytes_read;
        }

      bis.close();
      harness.check(total_read, str.length(), "total_read");

	  // Miscellaneous methods:
	  sbis = new StringBufferInputStream(str);
	  bis = new BufferedInputStream(sbis);
	  harness.check(bis.available(), str.length(), "available()");
	  harness.debug(bis.available() + " bytes available; should be " + str.length());
	  harness.check(bis.markSupported(), "markSupported()");
	  harness.debug("Mark " + (bis.markSupported() ? "is" : "is not") + " supported.");
	  int skip = 10;
	  long skipped = bis.skip(skip);
	  harness.check(skipped, skip, "skip(long)");
	  harness.debug("Skipped " + skipped + "(=" + skip + "?) bytes");
	  harness.debug("Reading " + bis.read(buf, 0, 3) + "(=3?) bytes");
	  String tst = new String(buf, 0, 3);
	  harness.check(tst, str.substring(skip, skip + 3), "read(buf[], off, len)");
	  harness.debug("Extracted " + tst + "; expected " + str.substring(skip, skip + 3));
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
} // main

} // class SimpleRead

