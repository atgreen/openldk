// Tags: JDK1.2

// Copyright (C) 1998 Cygnus Solutions

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

public class hash implements Testlet
{
  public void test (TestHarness harness)
    {
      char[] cstr = { 'a', 'b', 'c', '\t', 'A', 'B', 'C', ' ', '1', '2', '3' };

      String a = new String();
      String b = new String(" abc\tABC 123\t");
      String c = new String(new StringBuffer("abc\tABC 123"));

      /* These results are for JDK 1.2; the hashCode algorithm changed
	 from JDK 1.1.  */
      harness.check (a.hashCode(), 0);
      harness.check (b.hashCode(), -524164548);
      harness.check (c.hashCode(), -822419571);
    }
}
