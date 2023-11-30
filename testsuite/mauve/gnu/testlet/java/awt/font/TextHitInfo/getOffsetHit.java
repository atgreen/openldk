/* getoffsetHit.java 
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

public class getOffsetHit implements Testlet
{

  public void test(TestHarness harness)
  {
    TextHitInfo info = TextHitInfo.leading(0);
    harness.check(info.getOffsetHit(0).toString(), "TextHitInfo[0L]");
    harness.check(info.isLeadingEdge(), true);
    
    info = TextHitInfo.leading(0);
    harness.check(info.getOffsetHit(-2).toString(), "TextHitInfo[-2L]");
    harness.check(info.isLeadingEdge(), true);
    
    info = TextHitInfo.leading(0);
    harness.check(info.getOffsetHit(2).toString(), "TextHitInfo[2L]");
    harness.check(info.isLeadingEdge(), true);
    
    info = TextHitInfo.trailing(0);
    harness.check(info.getOffsetHit(0).toString(), "TextHitInfo[0T]");
    harness.check(info.isLeadingEdge(), false);
    
    info = TextHitInfo.trailing(0);
    harness.check(info.getOffsetHit(-2).toString(), "TextHitInfo[-2T]");
    harness.check(info.isLeadingEdge(), false);
    
    info = TextHitInfo.trailing(0);
    harness.check(info.getOffsetHit(2).toString(), "TextHitInfo[2T]");
    harness.check(info.isLeadingEdge(), false);
  }

}
