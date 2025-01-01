/*************************************************************************
/* Test.java -- Test LineNumberInputStream
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

package gnu.testlet.java.io.LineNumberInputStream;

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
      String str = "I grew up by a small town called Laconia, Indiana\r" +
         "which has a population of about 64 people.  But I didn't live\r\n" +
         "in town.  I lived on a gravel road about 4 miles away.\n" +
         "They paved that road\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      LineNumberInputStream lnis = new LineNumberInputStream(sbis);

      lnis.setLineNumber(2);

      byte[] buf = new byte[32];
      int bytes_read; 
      while ((bytes_read = lnis.read(buf)) != -1)
        {
          str = new String(buf, 0, bytes_read);
          if (str.indexOf("\r") != -1)
            {
              harness.debug("\nFound an unexpected \\r\n");
              harness.check(false);
            }
            
          harness.debug(str, false);
        }

      harness.check(lnis.getLineNumber(), 6, "getLineNumber - first test");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }

  try
    {
      String str = "One time I was playing kickball on the playground\n" +
         "in 4th grade and my friends kept talking about how they smelled\n" +
         "pot.  I kept asking them what they smelled because I couldn't\n" +
         "figure out how a pot could have a smell";

      StringBufferInputStream sbis = new StringBufferInputStream(str);
      LineNumberInputStream lnis = new LineNumberInputStream(sbis);

      byte[] buf = new byte[32];
      int bytes_read; 
      while ((bytes_read = lnis.read(buf)) != -1)
        harness.debug(new String(buf, 0, bytes_read), false);

      harness.debug("");
      harness.check(lnis.getLineNumber(), 3, "getLineNumber - second test");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // class Test

