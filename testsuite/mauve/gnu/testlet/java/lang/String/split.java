// Tags: JDK1.4

// Copyright (C) 2005 Mark J Wielaard (mark@klomp.org)

// This file is part of Mauve.

// Mauve is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.

// Mauve is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Mauve; see the file COPYING.  If not, write to
// the Free Software Foundation, 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

package gnu.testlet.java.lang.String;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.io.*;

/**
 * Tests for split bug based on a test submitted by Amit Jain
 * (amitrjain@hotmail.com) to GNU Classpath.
 */
public class split implements Testlet
{
  public void test (TestHarness harness)
  {
    String fullPath = "test.txt";
    String text
      = "A\tB\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\tC"
      + "\n"
      + "A\t\tB\t\t\t\t\t\t\t\t\tC\t"
      + "\n";

    String[] s1, s2;
    try
      {
	StringReader sr = new StringReader(text);
	BufferedReader r = new BufferedReader(sr);
	
	String row1 = r.readLine();
	s1 = row1.split("\t");
	String row2 = r.readLine();
	s2 = row2.split("\t");
	r.close();
      }
    catch (IOException ioe)
      {
	harness.debug(ioe);
	harness.check(false, ioe.toString());
	return;
      }

    harness.check(s1.length, 18);
    harness.check(s1[0], "A");
    harness.check(s1[1], "B");
    harness.check(s1[2], "");
    harness.check(s1[3], "");
    harness.check(s1[4], "");
    harness.check(s1[5], "");
    harness.check(s1[6], "");
    harness.check(s1[7], "");
    harness.check(s1[8], "");
    harness.check(s1[9], "");
    harness.check(s1[10], "");
    harness.check(s1[11], "");
    harness.check(s1[12], "");
    harness.check(s1[13], "");
    harness.check(s1[14], "");
    harness.check(s1[15], "");
    harness.check(s1[16], "");
    harness.check(s1[17], "C");

    // Note that trailing "empties" are discarded.
    harness.check(s2.length, 12);
    harness.check(s2[0], "A");
    harness.check(s2[1], "");
    harness.check(s2[2], "B");
    harness.check(s2[3], "");
    harness.check(s2[4], "");
    harness.check(s2[5], "");
    harness.check(s2[6], "");
    harness.check(s2[7], "");
    harness.check(s2[8], "");
    harness.check(s2[9], "");
    harness.check(s2[10], "");
    harness.check(s2[11], "C");

    String[] s3 = "hello, world".split("\uFFFF");
    harness.check(s3.length, 1);
    harness.check(s3[0], "hello, world");
  }
}
