/*************************************************************************
/* MarkReset.java -- Tests BufferedInputStream mark/reset functionality
/*
/* Copyright (c) 1998, 2005 Free Software Foundation, Inc.
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

public class MarkReset implements Testlet
{

public static int
marktest(InputStream ins, TestHarness harness) throws IOException
{
  BufferedInputStream bis = new BufferedInputStream(ins, 15);

  int bytes_read;  
  int total_read = 0;
  byte[] buf = new byte[12];

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bis.mark(75);
  bis.read();
  bis.read(buf);
  bis.read(buf);
  bis.read(buf);
  bis.reset();

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bis.mark(555);

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bis.reset();

  bis.read(buf);
  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  bis.mark(14);

  bis.read(buf);

  bis.reset();

  bytes_read = bis.read(buf);
  total_read += bytes_read;
  harness.debug(new String(buf, 0, bytes_read), false);

  while ((bytes_read = bis.read(buf)) != -1)
    {
      harness.debug(new String(buf, 0, bytes_read), false);
      total_read += bytes_read;
    }

  return(total_read);
}
  private static void readFully(InputStream in, int len) throws java.io.IOException
  {
    int nr;
    byte[] buf = new byte[len];
    while (len > 0) {
      if ((nr = in.read(buf, 0, len)) <= 0)
        throw new java.io.IOException("Unexpected EOF");
      len -= nr;
    }
  }

public void
test(TestHarness harness)
{
  try
    {
      harness.debug("First BufferedInputStream mark/reset series");
      harness.debug("Underlying InputStream does not support mark/reset");

      String str = "My 6th grade teacher was named Mrs. Hostetler.\n" +
        "She had a whole list of rules that you were supposed to follow\n" +
        "in class and if you broke a rule she would make you write the\n" +
        "rules out several times.  The number varied depending on what\n" +
        "rule you broke.  Since I knew I would get in trouble, I would\n" +
        "just go ahead and write out a few sets on the long school bus\n" +
        "ride home so that if had to stay in during recess and write\n" +
        "rules, five minutes later I could just tell the teacher I was\n" +
        "done so I could go outside and play kickball.\n";

      StringBufferInputStream sbis = new StringBufferInputStream(str);

      int total_read = marktest(sbis, harness);
      harness.check(total_read, str.length(), "total_read");
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
   
  try
    {
      harness.debug("Second BufferedInputStream mark/reset series");
      harness.debug("Underlying InputStream supports mark/reset");

      String str = "My first day of college was fun.  A bunch of us\n" +
         "got pretty drunk, then this guy named Rick Flake (I'm not\n" +
         "making that name up) took a piss in the bed of a Physical\n" +
         "Plant dept pickup truck.  Later on we were walking across\n" +
         "campus, saw a cop, and took off running for no reason.\n" +
         "When we got back to the dorm we found an even drunker guy\n" +
         "passed out in a shopping cart outside.\n";

      ByteArrayInputStream sbis = new ByteArrayInputStream(str.getBytes());

      int total_read = marktest(sbis, harness);
      harness.check(total_read, str.length(), "total_read");

    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }

  try
    {
      harness.checkPoint("Third BufferedInputStream mark/reset series");
      ByteArrayInputStream bais = new ByteArrayInputStream(new byte[100000]);
      BufferedInputStream bis = new BufferedInputStream(bais, 2048);
      bis.mark(2048);
      readFully(bis, 2049);
      harness.check(true);
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }

  try
    {
      harness.checkPoint("Forth BufferedInputStream mark/reset series");
      ByteArrayInputStream bais = new ByteArrayInputStream(new byte[100000]);
      BufferedInputStream bis = new BufferedInputStream(bais, 2048);
      bis.mark(2050);
      readFully(bis, 2050);
      bis.mark(2052);
      readFully(bis, 2052);
      harness.check(true);
    }
  catch (IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
} // main

} // class MarkReset

