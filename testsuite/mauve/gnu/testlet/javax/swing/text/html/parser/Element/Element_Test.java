// Tags: JDK1.2

// Copyright (C) 2005 Audrius Meskauskas, AudriusA@Bluewin.ch

// // This file is part of Mauve.

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

package gnu.testlet.javax.swing.text.html.parser.Element;

import gnu.testlet.TestHarness;
import gnu.testlet.Testlet;

import javax.swing.text.html.parser.*;
import javax.swing.text.html.parser.AttributeList;
import javax.swing.text.html.parser.DTDConstants;

/**
 * @author Audrius Meskauskas (AudriusA@Bluewin.ch)
 */
public class Element_Test
  implements Testlet
{
  TestHarness harness;

  public Element_Test()
  {
  }

  public void test(TestHarness a_harness)
  {
    harness = a_harness;
    try
      {
      testName2type();
      testAttributeGetter();
      }
    catch (Throwable exc)
      {
      exc.printStackTrace();
      if (exc != null)
        harness.fail(exc.getClass().getName() + ":" + exc.getMessage());
      else
        harness.fail("exception");
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

    Element e =
      DTD.getDTD("test").defineElement("e", 0, false, false, null, null, null,
                                       null
                                      );
    e.atts = head;

    for (int i = 0; i < 24; i++)
      {
      // Check if the name is found.
      harness.check(e.getAttribute("a" + i).toString(), "a" + i);

      // Check if the attribute value is correct.
      harness.check(e.getAttribute("a" + i).value, "v" + i);
      }

    // Check for unknown attribute
    harness.check(e.getAttribute("audrius"), null);

    // Check for unknown value
    harness.check(e.getAttributeByValue("audrius"), null);
  }

  public void testName2type()
  {
    harness.check(Element.name2type("CDATA"), DTDConstants.CDATA);
    harness.check(Element.name2type("RCDATA"), DTDConstants.RCDATA);
    harness.check(Element.name2type("EMPTY"), DTDConstants.EMPTY);
    harness.check(Element.name2type("ANY"), DTDConstants.ANY);

    harness.check(Element.name2type("audrius"), 0);
    harness.check(Element.name2type("rcdata"), 0);
  }

}
