/*************************************************************************
/* SimpleRead.java -- BufferedReader simple read test
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

public class SimpleRead implements Testlet
{

public void
test(TestHarness harness)
{
  try
    {
      String str = "My 5th grade teacher was named Mr. Thompson.  Terry\n" +
        "George Thompson to be precise.  He had these sideburns like\n" +
        "Isaac Asimov's, only uglier.  One time he had a contest and said\n" +
        "that if any kid who could lift 50lbs worth of weights on a barbell\n" +
        "all the way over their head, he would shave it off.  Nobody could\n" +
        "though.  One time I guess I made a comment about how stupid his\n" +
        "sideburns worked and he not only kicked me out of class, he called\n" +
        "my mother.  Jerk.\n";

      StringReader sr = new StringReader(str);
      BufferedReader br = new BufferedReader(sr, 15);

      char[] buf = new char[12];
      int chars_read, total_read = 0;
      while((chars_read = br.read(buf)) != -1)
        {
          harness.debug(new String(buf, 0, chars_read), false);
          total_read += chars_read;
        }

      br.close();
      harness.check(total_read, str.length(), "total_read");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
} // main

} // class SimpleRead

