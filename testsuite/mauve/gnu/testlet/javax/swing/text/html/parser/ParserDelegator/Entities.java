// Tags: JDK1.2

// Copyright (C) 2005, 2006 Audrius Meskauskas <audriusa@bluewin.ch>

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
// the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
// Boston, MA 02110-1301 USA.


package gnu.testlet.javax.swing.text.html.parser.ParserDelegator;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

/**
 * This entity test is known and must pass as early as since jdk 1.2, but it
 * fails with the older releases.
 * 
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Entities extends SimpleParsing
  implements Testlet
{

  public void test(TestHarness harness)
  {
    super.test(harness);
  }
  
  public void verify(String html, String trace)
  {
    verify(html, trace, html);
  }

  public void testHTMLParsing()
  {
    // Test entities.
    verify("hex: &#x55; eqdec: &#61; ampnamed: &amp;",
             "<html _implied_='true'><head _implied_='true'></head><body _implied_='true'>'hex: U eqdec: = ampnamed: &'</body></html>",
             "Entities"
            );
  }
}
