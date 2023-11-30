// Tags: JDK1.2 GNU
// Uses: TestCase

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


package gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser;

import gnu.javax.swing.text.html.parser.HTML_401F;
import gnu.javax.swing.text.html.parser.htmlAttributeSet;
import gnu.javax.swing.text.html.parser.support.parameterDefaulter;
import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.AttributeSet;
import javax.swing.text.html.HTML.Attribute;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class parameterDefaulter_Test
  extends TestCase
  implements Testlet
{
  parameterDefaulter defaulter;

  public void test(TestHarness harness)
  {
    h = harness;
    testDefaultValues();
  }

  public void testDefaultValues()
  {
    AttributeSet d;
    d = defaulter.getDefaultParameters("FrAmE");
    assertEquals(d.getAttribute("scrolling"), "auto");
    d = defaulter.getDefaultParameters("input");
    assertEquals(d.getAttribute("type"), "text");

    htmlAttributeSet hma = new htmlAttributeSet();
    hma.setResolveParent(d);
    hma.addAttribute("ku", "1");
    hma.addAttribute(Attribute.ACTION, "sleep");

    assertEquals(hma.getAttribute("action"), "sleep");
    assertEquals(hma.getAttribute(Attribute.ACTION), "sleep");
    assertEquals(hma.getAttribute("ku"), "1");

    // Calling the parent:
    assertEquals(hma.getAttribute(Attribute.TYPE), "text");

    d = defaulter.getDefaultParameters("audrius");
    assertEquals(d.getAttribute("scrolling"), null);
  }

  protected void setUp()
  {
    defaulter = new parameterDefaulter(HTML_401F.getInstance());
  }

  protected void tearDown()
                   throws java.lang.Exception
  {
    defaulter = null;
    super.tearDown();
  }
}
