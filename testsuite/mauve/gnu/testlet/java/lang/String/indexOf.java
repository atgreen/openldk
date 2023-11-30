// Tags: JDK1.0

// Copyright (C) 1998, 1999 Cygnus Solutions

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

public class indexOf implements Testlet
{
  public void test (TestHarness harness)
    {
      char[] cstr = { 'a', 'b', 'c', '\t', 'A', 'B', 'C', ' ', '1', '2', '3' };

      String b = new String(" abc\tABC 123\t");
      String d = new String(cstr);

      harness.check (b.indexOf(' '), 0);
      harness.check (b.indexOf(' ', 1), 8);
      harness.check (b.indexOf(' ', 10), -1);
      harness.check (b.indexOf(' ', -1), 0);
      harness.check (b.indexOf(' ', b.length()), -1);
      harness.check (b.indexOf("abc"), 1);
      harness.check (b.indexOf("abc", 1), 1);
      harness.check (b.indexOf("abc", 10), -1);

      harness.check ("".indexOf(""), 0);
      harness.check (b.indexOf(""), 0);
      harness.check ("".indexOf(b), -1);

      harness.check (b.lastIndexOf(' '), 8);
      harness.check (b.lastIndexOf(' ', 1), 0);
      harness.check (b.lastIndexOf(' ', 10), 8);
      harness.check (b.lastIndexOf(' ', -1), -1);
      harness.check (b.lastIndexOf(' ', b.length()), 8);
      harness.check (b.lastIndexOf("abc"), 1);
      harness.check (b.lastIndexOf("abc", 1), 1);
      harness.check (b.lastIndexOf("abc", 10), 1);

      harness.check ("".lastIndexOf(""), 0);
      harness.check (b.lastIndexOf(""), b.length());
      harness.check ("".lastIndexOf(b), -1);
    }
}
