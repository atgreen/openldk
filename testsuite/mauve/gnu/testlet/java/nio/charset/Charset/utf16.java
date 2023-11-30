// Tags: JDK1.4

// Copyright (C) 2004 Free Software Foundation, Inc.
// Written by Michael Koch (konqueror@gmx.de)

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

package gnu.testlet.java.nio.charset.Charset;

import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

import java.nio.charset.*;

public class utf16 implements Testlet
{
  private void testCharset(TestHarness h, String name,
			   float expected_average, float expected_max)
  {
    Charset charset = Charset.forName(name);
    h.check(charset != null, "Charset.forName(\"" + name + "\") returned 'null'");

    CharsetEncoder encoder = charset.newEncoder();
    h.check(encoder != null, "Charset.newEncoder() returned 'null'");

    float average = encoder.averageBytesPerChar();
    h.check(average, expected_average,
	    "average bytes per char (expected: " + expected_average + ", got: " + average + ")");
    
    float max = encoder.maxBytesPerChar();
    h.check(max, expected_max,
	    "max bytes per char (expected: " + expected_max + ", got: " + max + ")");
  }
  
  public void test(TestHarness h)
  {
    testCharset(h, "UTF-16", 2.0f, 4.0f);
    testCharset(h, "UTF-16LE", 2.0f, 2.0f);
    testCharset(h, "UTF-16BE", 2.0f, 2.0f);
  }
}
