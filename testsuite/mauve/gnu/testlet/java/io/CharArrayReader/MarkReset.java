/*************************************************************************
/* MarkReset.java -- Test CharArrayReader mark/reset functionality
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

package gnu.testlet.java.io.CharArrayReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class MarkReset extends CharArrayReader implements Testlet
{

public 
MarkReset(char[] c)
{
  super(c);
}

// Constructor for test suite
public
MarkReset()
{
  super(new char[1]);
}

public void
test(TestHarness harness)
{
  String str = "In junior high, I did a lot writing.  I wrote a science\n" +
     "fiction novel length story that was called 'The Destruction of\n" +
     "Planet Earth'.  All the characters in the story were my friends \n" +
     "from school because I couldn't think up any cool names.";

  char[] str_chars = new char[str.length()];
  str.getChars(0, str.length(), str_chars, 0);

  MarkReset car = new MarkReset(str_chars);
  char[] read_buf = new char[12];

  try
    {
      car.read(read_buf);      
      harness.check(car.ready(), "ready()");
      harness.check(car.skip(5), 5, "skip()");
      harness.check(car.markSupported(), "markSupported()");
    
      car.mark(0);
      int pos_save = car.pos;
      car.read();
      car.reset();
      harness.check(car.pos, pos_save, "reset()");
    }
  catch(IOException e)
    {
      harness.debug(e);
      harness.check(false);
    }
}

} // MarkReset

