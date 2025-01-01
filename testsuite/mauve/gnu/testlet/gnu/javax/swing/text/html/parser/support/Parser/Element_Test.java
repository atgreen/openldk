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

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTD;
import javax.swing.text.html.parser.DTDConstants;
import javax.swing.text.html.parser.Element;

/**
 * @author Audrius Meskauskas (AudriusA@Bioinformatics.org)
 */
public class Element_Test
  extends TestCase
  implements Testlet
{
  private Element element = null;

  public void test(TestHarness harness)
  {
    h = harness;
    try
      {
        testAttributeGetter();
        testName2type();
      }
    catch (Exception ex)
      {
        h.fail("Exception " + ex);
      }
  }

  public void testAttributeGetter()
                           throws Exception
  {
    // Create a chain of 24 attributes:
    AttributeList list = new AttributeList("heading");
    AttributeList head = list;
    list.value = null;
    for (int i = 0; i < 24; i++)
      {
        AttributeList a = new AttributeList("a" + i);
        a.value = "v" + i;
        list.next = a;
        list = a;
      }

    Element e = DTD.getDTD("test").getElement("e");
    e.atts = head;

    for (int i = 0; i < 24; i++)
      {
        // Check if the name is found.
        assertEquals(e.getAttribute("a" + i).toString(), "a" + i);

        // Check if the attribute value is correct.
        assertEquals(e.getAttribute("a" + i).value, "v" + i);

        // Check if the attribute can be found by value.
        assertEquals(e.getAttributeByValue("v" + i).name, "a" + i);
      }

    // Check is the null value is searched correctly.
    assertEquals(e.getAttributeByValue(null).toString(), "heading");

    // Check for unknown attribute
    assertEquals(e.getAttribute("audrius"), null);

    // Check for unknown value
    assertEquals(e.getAttributeByValue("audrius"), null);
  }

  public void testName2type()
  {
    assertEquals(Element.name2type("CDATA"), DTDConstants.CDATA);
    assertEquals(Element.name2type("RCDATA"), DTDConstants.RCDATA);
    assertEquals(Element.name2type("EMPTY"), DTDConstants.EMPTY);
    assertEquals(Element.name2type("ANY"), DTDConstants.ANY);

    assertEquals(Element.name2type("audrius"), 0);
    assertEquals(Element.name2type("rcdata"), 0);
  }

  protected void setUp()
                throws Exception
  {
    super.setUp();
  }

  protected void tearDown()
                   throws Exception
  {
    element = null;
    super.tearDown();
  }
}
