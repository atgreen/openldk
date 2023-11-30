/*************************************************************************
/* OutOfBounds.java -- CharArrayReader exception tests.
/*
/* Copyright (c) 2003 Free Software Foundation, Inc.
/* Written by Guilhem Lavaux (guilhem@kaffe.org)
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

public class OutOfBounds implements Testlet
{
  public void test(TestHarness harness)
  {
    String str = "In junior high, I did a lot writing.  I wrote a science\n" +
      "fiction novel length story that was called 'The Destruction of\n" +
      "Planet Earth'.  All the characters in the story were my friends \n" +
      "from school because I couldn't think up any cool names.\n";

    char[] str_chars = new char[str.length()];
    str.getChars(0, str.length(), str_chars, 0);
    char[] read_buf = new char[12];
    
    CharArrayReader car = new CharArrayReader(str_chars);
    
    harness.checkPoint("read(X) should throw IndexOutOfBoundsException");
    
    // Test #1
    try
      {
	car.read(read_buf, 0, read_buf.length+1);
	harness.check(false);
      }
    catch (IndexOutOfBoundsException e)
      {
	harness.check(true);
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }

    // Test #2
    try
      {
	car.read(read_buf, read_buf.length, 1);
	harness.check(false);
      }
    catch (IndexOutOfBoundsException e)
      {
	harness.check(true);
      }
    catch (Exception e)
      {
	harness.debug(e);
	harness.check(false);
      }
  }
}
