/*************************************************************************
/* BasicTests.java -- CharArrayWriter basic tests.
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

public class BasicTests implements Testlet
{

public void
test(TestHarness harness)
{
  /*
   * Use several methods to write to the buffer
   * and verify that the results are correct.
   */
  String firstLines = "The first lines\n" +
    "of the test which include \uA000 inverted question\n" +
    "and \u6666 e-with-hat";
  String thirdLine = "a third line";
  String expected = firstLines + ' ' + "third";
  CharArrayWriter writer = new CharArrayWriter();
  if (writer.size() != 0)
    harness.check(writer.size(), 0, "empty size");
  char[] thirdLineArray = new char[thirdLine.length()];
  String extractedString;
  try
    {
      writer.write(firstLines, 0, firstLines.length());
      writer.write(32);
      thirdLine.getChars(0, thirdLine.length(), thirdLineArray, 0);
      writer.write(thirdLineArray, 2, 5);
      extractedString = writer.toString();
      harness.check(extractedString, expected, "basic string");
      /*
       * Clear the buffer and write some more, then see if
       * toCharArray works.
       */
      writer.reset();
      writer.write(thirdLine, 0, thirdLine.length());
    }
  catch (Throwable t)
    {
      harness.debug(t);
      harness.check(false, "Unexpected exception");
      extractedString = "";
    }
  char[] resultArray = writer.toCharArray();
  boolean arrayEquals = resultArray.length == thirdLineArray.length;
  if (arrayEquals)
    for (int i=0; i < resultArray.length; i++)
      if (resultArray[i] != thirdLineArray[i])
        arrayEquals = false;
  harness.checkPoint("reset string");
  harness.check(arrayEquals);
  /*
   * Try flush and close and make sure they do nothing.
   */
  try
    {
      writer.flush();
      writer.close();
    }
  catch (Throwable t)
    {
      harness.debug(t);
      harness.check(false, "Unexpected exception flush/close");
    }
  extractedString = writer.toString();
  harness.check(extractedString, thirdLine, "flush and close");
  /*
   * Make another CharArrayWriter and writeTo it.
   */
  CharArrayWriter secondWriter = new CharArrayWriter();
  boolean pass = false;
  try {
      writer.writeTo(secondWriter);
      extractedString = secondWriter.toString();
      if (extractedString.equals(thirdLine))
	  pass = true;
  } catch (IOException ie) {
    // Nothing need be done, it is enough for pass to remain false
  }
  harness.checkPoint("writeTo");
  harness.check(pass);
}

} // BasicTests
