// Tags: JDK1.0

// Copyright (C) 1999 Cygnus Solutions
// Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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
import java.io.UnsupportedEncodingException;

public class getBytes implements Testlet
{
  public void test (TestHarness harness)
  {
    String s = new String ("test me");
    try
      {
	byte[] b = s.getBytes("8859_1");
	harness.check (b.length, s.length());

	b = s.substring(0, 4).getBytes("8859_1");
	harness.check (b.length, 4);

	b = s.substring(5, 7).getBytes("8859_1");
	harness.check (b.length, 2);

	s = new StringBuffer("abcdefghijklmnopqrstuvwxyz")
		.append(Integer.toString(123456789))
		.toString().substring(10,30);
	b = s.getBytes("8859_1");
	harness.check (b.length, 20);
	b = s.getBytes("UTF8");
	harness.check (b.length, 20);
      }
    catch (UnsupportedEncodingException _)
      {
	harness.check (false);
      }
  }
}
