// Tags: JDK1.5

// Copyright (C) 2008 Andrew John Hughes

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

package gnu.testlet.java.lang.StringBuffer;
import gnu.testlet.Testlet;
import gnu.testlet.TestHarness;

public class PR34840
  implements Testlet
{

  public void test(TestHarness harness)
  {
    StringBuffer sb = new StringBuffer();
    sb.append("He");
    sb.append((CharSequence) null,2,4);
    sb.append("o, wor");
    sb.append((CharSequence) null,2,3);
    sb.append("d!");
    harness.check(sb.toString(), "Hello, world!", "Appendable PR34840 check");
  }

}
