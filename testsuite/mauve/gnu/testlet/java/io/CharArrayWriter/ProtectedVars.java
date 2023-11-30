/*************************************************************************
/* ProtectedVars.java -- Test CharArrayWriter protected variables
/*
/* Copyright (c) 2002 Free Software Foundation, Inc.
/* Written by David J. King (david_king@softhome.net)
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

package gnu.testlet.java.io.CharArrayWriter;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;
import java.io.*;

public class ProtectedVars extends CharArrayWriter implements Testlet
{

// Constructor for the test suite
public
ProtectedVars()
{
  super();
}

public void
test(TestHarness harness)
{
  /*
   * Put in a string, and see if the count is correct.
   */
  String str = "Here is a test string";
  ProtectedVars writer = new ProtectedVars();
  try
    {
      // Inside try-catch block since some implementations throw IOException.
      writer.write(str, 0, str.length());
    }
  catch (Throwable t)
    {
      harness.debug(t);
      harness.check(false, "Unexpected exception");
    }
  harness.check(writer.count, str.length(), "count");
  /*
   * Then see if the stored buffer is correct.
   */
  char[] strArray = new char[str.length()];
  str.getChars(0, str.length(), strArray, 0);
  boolean pass = writer.buf.length >= strArray.length;
  if (pass)
    for (int i=0; i < writer.count; i++)
      if (writer.buf[i] != strArray[i])
        pass = false;
  harness.checkPoint("buffer");
  harness.check(pass);
}

} // ProtectedVars
