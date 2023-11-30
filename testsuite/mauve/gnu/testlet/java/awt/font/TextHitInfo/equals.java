/* equals.java 
   Copyright (C) 2006 Red Hat
This file is part of Mauve.

Mauve is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Mauve is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Mauve; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

*/

// Tags: JDK1.2

package gnu.testlet.java.awt.font.TextHitInfo;

import java.awt.font.TextHitInfo;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

public class equals implements Testlet
{

  public void test(TestHarness harness)
  {
    equalsObject(harness);
    equalsTextHitInfo(harness);
  }
  
  public void equalsObject(TestHarness harness)
  {
    TextHitInfo info = TextHitInfo.trailing(0);
    harness.check(info.equals((Object) null), false);

    harness.check(info.equals((Object) TextHitInfo.trailing(0)), true);
    
    String a = "Some String";
    harness.check(info.equals(a), false);
  }

  public void equalsTextHitInfo(TestHarness harness)
  {
    TextHitInfo info = TextHitInfo.trailing(0);
    harness.check(info.equals((TextHitInfo) null), false);

    harness.check(info.equals(TextHitInfo.trailing(0)), true);
    harness.check(info.equals(TextHitInfo.trailing(1)), false);
    harness.check(info.equals(TextHitInfo.leading(0)), false);
    harness.check(info.equals(TextHitInfo.leading(1)), false);
  }
}
