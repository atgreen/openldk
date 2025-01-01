/*************************************************************************
/* Unread.java -- PushbackInputStream basic unread tests.
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

package gnu.testlet.java.io.PushbackInputStream;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class Unread implements Testlet
{

public void
test(TestHarness harness)
{
  String str = "Once when I was in fourth grade, my friend Lloyd\n" +
    "Saltsgaver and I got in trouble for kicking a bunch of\n" +
    "Kindergartners off the horse swings so we could play a game\n" +
    "of 'road hog'\n";

  try
    {
      PushbackInputStream pist = new PushbackInputStream(
        new StringBufferInputStream(str), 15);

	  harness.check(pist.available(), str.length(), "available()");
	  harness.check(!pist.markSupported(), "markSupported()");      

      byte[] read_buf1 = new byte[12]; 
      byte[] read_buf2 = new byte[12]; 
      
      pist.read(read_buf1); 
      pist.unread(read_buf1);
      pist.read(read_buf2);
      
      for (int i = 0; i < read_buf1.length; i++)
        {
          if (read_buf1[i] != read_buf2[i])
            throw new IOException("Re-reading bytes gave different results");
        }

      pist.unread(read_buf2, 1, read_buf2.length - 1);      
      pist.unread(read_buf2[0]);

      int bytes_read, total_read = 0;
      while ((bytes_read = pist.read(read_buf1)) != -1)
        {
          harness.debug(new String(read_buf1, 0, bytes_read), false);
          total_read += bytes_read;
        }

      harness.debug(str);
      harness.check(total_read, str.length(), "total_read == str.length()");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // class Unread

