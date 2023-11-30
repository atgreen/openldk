// Tags: JDK1.0

// Copyright (C) 2002 Free Software, Inc.

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
// Boston, MA 02111-1307, USA.  */

package gnu.testlet.java.lang.String;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

/**
 * Triggers old equals bug in Classpath when two Strings are different
 * substrings of the same length of the same bigger String.
 */
public class equals implements Testlet
{
  public void test (TestHarness harness)
  {
    String s1 = "Hello World";
    String s2 = s1.substring(0,4);
    String s3 = s1.substring(6,10);
    harness.check(! s1.equals(s2));
    harness.check(! s2.equals(s1));
    harness.check(! s1.equals(s3));
    harness.check(! s3.equals(s1));
    harness.check(! s2.equals(s3));
    harness.check(! s3.equals(s2));
  }
}
