/*************************************************************************
/* BufferOverflow.java - Test PushbackReader buffer overflows
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

package gnu.testlet.java.io.PushbackReader;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class BufferOverflow implements Testlet
{

public void
test(TestHarness harness)
{
  String str = "I used to idolize my older cousin Kurt.  I wanted to be\n" +
    "just like him when I was a kid.  (Now we are as different as night\n" +
    "and day - but still like each other).  One thing he did for a while\n" +
    "was set traps for foxes thinking he would make money off sellnig furs.\n" +
    "Now I never saw a fox in all my years of Southern Indiana.  That\n" +
    "didn't deter us.  One time we went out in the middle of winter to\n" +
    "check our traps.  It was freezing and I stepped onto a frozen over\n" +
    "stream. The ice broke and I got my foot soak.  Despite the fact that\n" +
    "it made me look like a girl, I turned around and went straight home.\n" +
    "Good thing too since I couldn't even feel my foot by the time I got\n" +
    "there.\n";

  try
    {
      PushbackReader prt = new PushbackReader(new StringReader(str), 10);

      char[] read_buf = new char[12]; 

      prt.read(read_buf);
      prt.unread(read_buf);
      harness.debug("Did not throw expected buffer overflow exception");
      harness.check(false);
    }
  catch(IOException e)
    {
      harness.check(true); // Yes, we expect this exception
    }
}

} // class BufferOverflow

