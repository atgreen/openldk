// Tags: JDK1.2
// Uses: ../../../../../../gnu/javax/swing/text/html/parser/support/Parser/TestCase

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


package gnu.testlet.javax.swing.text.html.parser.DTD;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;
import gnu.testlet.gnu.javax.swing.text.html.parser.support.Parser.TestCase;

import javax.swing.text.html.HTML;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.Element;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class DtdTest2
  extends TestCase
  implements Testlet
{
  static class D
    extends DTD
  {
    public D()
    {
      super("audrius");
    }

    public Element createElement(String n)
    {
      return getElement(n);
    }
  }

  public void test(TestHarness harness)
  {
    h = harness;
    testGetElement();
  }

  public void testGetElement()
  {
    D d = new D();
    HTML.Tag[] tags = HTML.getAllTags();

    Element prehead = d.createElement("head");

    for (int i = 0; i < tags.length; i++)
      {
        Element e = d.createElement(tags [ i ].toString());
        String name = tags [ i ].toString();
        assertNotNull("Element creation", e);
        assertTrue("Element name", e.getName().equalsIgnoreCase(name));
      }

    // Test upper/lowercase
    Element e = d.createElement("head");

    assertNotNull("Element creation", e);
    assertTrue("Element name", e.getName().equalsIgnoreCase("head"));
    assertEquals(HTML.Tag.HEAD, HTML.getTag(e.name));
    assertEquals("Field assignment", d.head, e);

    assertEquals(prehead, e);
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    super.tearDown();
  }
}
