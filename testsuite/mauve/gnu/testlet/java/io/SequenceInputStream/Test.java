/*************************************************************************
/* Test.java -- Run tests of SequenceInputStream
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

package gnu.testlet.java.io.SequenceInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class Test implements Testlet
{

public void
test(TestHarness harness)
{
  String str1 = "I don't believe in going to chain restaurants.  I think\n" +
    "they are evil.  I can't believe all the suburban folks who go to \n";

  String str2 = "places like the Olive Garden.  Not only does the food make\n" +
    "me want to puke, none of these chains has the slightest bit of character.\n";

  byte[] buf = new byte[10];

  try
    {
      StringBufferInputStream is1 = new StringBufferInputStream(str1);
      ByteArrayInputStream is2 = new ByteArrayInputStream(str2.getBytes());
      SequenceInputStream sis = new SequenceInputStream(is1, is2);

      int bytes_read;
	  harness.check(str1.length(), sis.available(), "available()");
      while((bytes_read = sis.read(buf)) != -1)
        {
          harness.debug(new String(buf,0,bytes_read), false);
        }

      sis.close();
      harness.check(true);
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }

  try
    { 
      StringBufferInputStream is1 = new StringBufferInputStream(str1);
      ByteArrayInputStream is2 = new ByteArrayInputStream(str2.getBytes());
      SequenceInputStream sis = new SequenceInputStream(is1, is2);

      sis.read(buf, 0, 5);
	  harness.check(true, "read(buf, off, len)");

      sis.close();

      harness.check(sis.read(), -1, "close() test");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // class Test

